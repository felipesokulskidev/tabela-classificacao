module Main (main) where

import Tipos -- traz Partida, Linha e os outros tipos

-- Partida para teste de compilação
exemploPartida :: Partida
exemploPartida = Partida 1 "Vasco" 7 1 "Athletico"

-- linha da tabela de teste
exemploLinha :: Linha
exemploLinha = Linha "Vasco" 5 5 0 0 30 1 -- Trem bala da Colina

main :: IO ()
main = do
    print exemploPartida  -- a partida inteira
    print (mandante exemploPartida)  -- "Vasco"
    print (golsVisitante exemploPartida) -- 1

    print exemploLinha  -- a linha inteira
    print (time exemploLinha) -- "Vasco"
    print (vitorias exemploLinha)   -- 5

    print (Vitoria == Empate)  -- False
