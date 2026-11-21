module Leitura where

import Tipos

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