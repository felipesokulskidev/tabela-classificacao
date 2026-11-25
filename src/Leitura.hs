module Leitura where

import Tipos
import Text.Read (readMaybe)
import Data.List (nub, (\\))
import Classificacao (pontosLinha, saldo)
import Text.Printf (printf)


-- erros possiveis na leitura do texto
data Erro
    = CamposInvalidos Int -- linha sem exatamente 5 campos
    | NumeroInvalido Int -- linha com rodada ou gols que nao sao numero
    | PlacarNegativo Int -- linha com gols negativos
    | MesmoTime Int -- linha com um time jogando contra si mesmo
    | TimeRepetidoNaRodada Int Time -- rodada em que o time aparece mais de uma vez
    | NomeVazio Int -- linha com nome de time vazio
    | RodadaInvalida Int -- linha com rodada <= 0
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
    if golsM < 0 || golsV < 0 then Left (PlacarNegativo n)  else Right ()
    if m == v                  then Left (MesmoTime n)       else Right ()
    if null m || null v        then Left (NomeVazio n)        else Right ()
    if rod <= 0                then Left (RodadaInvalida n)   else Right ()
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

-- completa o txt c espaç a direita ate ter n chars
preencher :: Int -> String -> String
preencher n texto = texto ++ replicate (n - length texto) ' '

-- tabela pronta p/ printar header + 1 linha p/ cada time
tabelaFormatada :: [Linha] -> String
tabelaFormatada linhas = unlines (cabecalho : zipWith formatarLinha [1 :: Int ..] linhas)
    where
        cabecalho = montar "Pos" "Time" ["Pts", "J", "V", "E", "D", "SG"]
        formatarLinha pos l = montar (show pos) (time l) (map show [pontosLinha l, jogos l, vitorias l, empates l, derrotas l, saldo l])
        montar pos nome numeros = preencher 5 pos ++ preencher 14 nome ++ concatMap (preencher 5) numeros

-- texto legivel pra cada erro, no lugar do show
mensagemErro :: Erro -> String
mensagemErro (CamposInvalidos n) = "Linha " ++ show n ++ ": a linha precisa ter 5 campos separados por ';'"
mensagemErro (NumeroInvalido n) = "Linha " ++ show n ++ ": a rodada ou o placar nao e um numero"
mensagemErro (PlacarNegativo n) = "Linha " ++ show n ++ ": o placar nao pode ser negativo"
mensagemErro (MesmoTime n)              = "Linha " ++ show n ++ ": um time nao pode jogar contra ele mesmo"
mensagemErro (TimeRepetidoNaRodada r t) = "Rodada " ++ show r ++ ": " ++ t ++ " aparece em mais de uma partida"
mensagemErro (NomeVazio n)              = "Linha " ++ show n ++ ": o nome do time nao pode ser vazio"
mensagemErro (RodadaInvalida n)         = "Linha " ++ show n ++ ": o numero da rodada deve ser maior que zero"

-- partida no formato "Vasco 7 x 1 AthleticoPR (rodada 1)"
formatarPartida :: Partida -> String
formatarPartida p = mandante p ++ " " ++ show (golsMandante p) ++ " x " ++ show (golsVisitante p) ++ " " ++ visitante p ++ " (rodada " ++ show (rodada p) ++ ")"

-- numero com 2 casas decimais 
duasCasas :: Double -> String
duasCasas = printf "%.2f"