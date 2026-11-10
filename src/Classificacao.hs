module Classificacao where

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