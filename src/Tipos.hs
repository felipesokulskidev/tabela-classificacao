module Tipos where

type Time = String -- Time é outro nome para String

data Resultado -- data cria um tipo novo, Resultado nome do tipo
    = Vitoria -- o "=" indica que os possiveis valores vem por aí
    | Empate
    | Derrota
    deriving (Show, Eq)   -- Show: permite imprimir; Eq: permite comparar com ==

data Partida = Partida
    { rodada :: Int,   -- número da rodada
     mandante :: Time,  -- time que joga em casa
     golsMandante :: Int,  -- gols do mandante
     golsVisitante :: Int,   -- gols do visitante
     visitante :: Time  -- time que joga fora
    } deriving (Show, Eq)    -- libera print e == para Partida

-- Semelhante ao struct de C
data Linha = Linha {
    time :: Time,  -- nome do time
    jogos :: Int,   -- partidas disputadas
    vitorias :: Int,   -- quantidade de vitórias
    empates :: Int,   -- quantidade de empates
    derrotas :: Int,   -- quantidade de derrotas
    golsPro :: Int,   -- gols que o time marcou
    golsContra :: Int    -- gols que o time sofreu
  } deriving (Show, Eq)  -- libera print e == para Linha