module Leitura where

import Tipos
import Text.Read (readMaybe)
import Data.List (nub, (\\))


-- erros possiveis na leitura do texto
data Erro
    = CamposInvalidos Int -- linha sem exatamente 5 campos
    | NumeroInvalido Int -- linha com rodada ou gols que nao sao numero
    | PlacarNegativo Int -- linha com gols negativos
    | MesmoTime Int -- linha com um time jogando contra si mesmo
    | TimeRepetidoNaRodada Int Time -- rodada em que o time aparece mais de uma vez
    deriving (Show, Eq) -- Show: permite imprimir; Eq: permite comparar com ==

-- quebra o texto em pedacos sempre que encontra o separador
separar :: Char -> String -> [String]
separar sep texto = foldr passo [""] texto
    where
        passo c acc
            | c == sep  = "" : acc -- achou o separador, então comeca um campo novo
            | otherwise = case acc of
                (atual : resto) -> (c : atual) : resto -- gruda a letra no campo atual
                []              -> [[c]] -- nunca acontece, mas o -Wall exige
--Função auxiliar de lerPartida
lerNumero :: Int -> String -> Either Erro Int
lerNumero n s = maybe (Left(NumeroInvalido n)) Right (readMaybe s)


lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
    (r, m, gm, gv, v) <- cincoCampos (separar ';' linha)
    rod   <- lerNumero n r
    golsM <- lerNumero n gm
    golsV <- lerNumero n gv
    if golsM < 0 || golsV < 0 then Left (PlacarNegativo n) else Right ()
    if m == v then Left (MesmoTime n) else Right ()
    return (Partida rod m golsM golsV v)
    where
        cincoCampos [a, b, c, d, e] = Right (a, b, c, d, e)
        cincoCampos _               = Left (CamposInvalidos n)

-- numera as linhas e joga fora as vazias e comentarios
linhasUteis :: String -> [(Int, String)]
linhasUteis texto = filter (ehUtil . snd) (zip [1 ..] (lines texto))
    where
        ehUtil linha = not (null linha) && take 1 linha /= "#"

-- ve se algum time aparece duas vezes na mesma rodada
verificarRodadas :: [Partida] -> Either Erro ()
verificarRodadas partidas = case repetidos of
    []            -> Right ()
    ((rod, nomeTime) : _)  -> Left (TimeRepetidoNaRodada rod nomeTime)
    where
        pares     = concatMap (\p -> [(rodada p, mandante p), (rodada p, visitante p)]) partidas
        repetidos = pares \\ nub pares
-- lê o texto todo, parando no primeiro erro
lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato texto = do
    partidas <- traverse (uncurry lerPartida) (linhasUteis texto)
    verificarRodadas partidas
    return partidas


-- relatório com todos os erros do txt, e n só o 1o
todosOsErros :: String -> [Erro]
todosOsErros texto = errosLinhas ++ errosRodadas
    where
        resultados   = [lerPartida n linha | (n, linha) <- linhasUteis texto] -- tenta ler cada linha
        errosLinhas  = [e | Left e <- resultados] -- linhas c erro
        partidas     = [p | Right p <- resultados] -- linhas validas
        pares        = concatMap (\p -> [(rodada p, mandante p), (rodada p, visitante p)]) partidas
        errosRodadas = [TimeRepetidoNaRodada r t | (r, t) <- nub (pares \\ nub pares)]