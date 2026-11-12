module Classificacao where

import Data.List (sortBy, maximumBy)
import Data.Ord (comparing, Down(..))
import Tipos



-- retorna os pontos de acordo com o resultado
pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

-- monta a linha de um time acumulando todas as partidas com foldl
linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime ps t = foldl somarPartida inicial ps
    where
        inicial = Linha t 0 0 0 0 0 0
        somarPartida l p = case resultadoPara t p of
            Nothing      -> l
            Just Vitoria -> l { jogos      = jogos l + 1
                              , vitorias   = vitorias l + 1
                              , golsPro    = golsPro l + golsFeitos t p
                              , golsContra = golsContra l + golsSofridos t p
                              }
            Just Derrota -> l { jogos      = jogos l + 1
                              , derrotas   = derrotas l + 1
                              , golsPro    = golsPro l + golsFeitos t p
                              , golsContra = golsContra l + golsSofridos t p
                              }
            Just Empate  -> l { jogos      = jogos l + 1
                              , empates    = empates l + 1
                              , golsPro    = golsPro l + golsFeitos t p
                              , golsContra = golsContra l + golsSofridos t p
                              }



-- pts ganhos e saldo de gols d uma linha da table
pontosLinha, saldo :: Linha -> Int
pontosLinha l = vitorias l * pontos Vitoria + empates l * pontos Empate -- 3 pontos p/ vitória e 1 p/ empate (reaproveita a func pontos)
saldo l = golsPro l - golsContra l -- gols pró - gols contra


-- % dos pontos disputados q o time conquistou (0 se n jogou)
aproveitamento :: Linha -> Double
aproveitamento l
    | jogos l == 0 = 0
    | otherwise    = fromIntegral (pontosLinha l) / fromIntegral (jogos l * pontos Vitoria) * 100


-- tabela completa 1 linha p/ cada time, order pelos crit de desempate
classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy criterios (map (linhaDoTime ps) (times ps))
    where
        -- o <> só passa pr prox crit se o d antes empatar
        criterios = comparing (Down . pontosLinha)  -- 1o + pts
                 <> comparing (Down . vitorias) -- 2o + vitórias
                 <> comparing (Down . saldo) -- 3o mlr saldo d gols
                 <> comparing (Down . golsPro) -- 4o + gols pro



-- só as partidas até a rodada n 
ateRodada :: Int -> [Partida] -> [Partida]
ateRodada n ps = filter ateN ps
    where
        ateN p = rodada p <= n -- a partida fica se a rodada dela é <= n

--funcao de diferenca usada por maiorGoleada
diferenca :: Partida -> Int
diferenca p = abs(golsMandante p - golsVisitante p)

maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada [] = Nothing
maiorGoleada ps = Just (maximumBy(comparing diferenca) ps)





