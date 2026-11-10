module Dados where

import Tipos -- daqui vem a partida

campeonatoTeste :: [Partida] -- 6 times, 5 rodadas
campeonatoTeste =
    -- Rodada 1
    [ Partida 1 "Vasco"       7 1 "AthleticoPR"   -- goleada
    , Partida 1 "Corinthians" 0 0 "Coritiba"      -- empate 0x0
    , Partida 1 "Fluminense"  2 1 "Flamengo"
    -- Rodada 2
    , Partida 2 "Vasco"       1 1 "Coritiba"
    , Partida 2 "AthleticoPR" 2 0 "Flamengo"
    , Partida 2 "Corinthians" 2 1 "Fluminense"
    -- Rodada 3
    , Partida 3 "Vasco"       0 2 "Flamengo"
    , Partida 3 "Coritiba"    1 1 "Fluminense"
    , Partida 3 "AthleticoPR" 1 3 "Corinthians"
    -- Rodada 4
    , Partida 4 "Vasco"       2 0 "Fluminense"
    , Partida 4 "Flamengo"    1 1 "Corinthians"
    , Partida 4 "Coritiba"    2 0 "AthleticoPR"
    -- Rodada 5
    , Partida 5 "Vasco"       1 2 "Corinthians"
    , Partida 5 "Fluminense"  3 1 "AthleticoPR"
    , Partida 5 "Flamengo"    0 1 "Coritiba"
    ]