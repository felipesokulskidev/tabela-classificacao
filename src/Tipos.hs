module Tipos where

import Data.List (nub, sort)

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

-- verdadeiro se o time jogou a partida
participou :: Time -> Partida -> Bool
participou t p = mandante p == t || visitante p == t

-- resultado da partida variando de acordo com o time
resultadoPara :: Time -> Partida -> Maybe Resultado -- Maybe pq pode não ter resultado
resultadoPara t p 
    | not (participou t p) = Nothing
    | feitos > sofridos = Just Vitoria
    | sofridos > feitos = Just Derrota  
    | otherwise = Just Empate
    where 
        feitos = if t == mandante p then golsMandante p else golsVisitante p
        sofridos = if t == mandante p then golsVisitante p else golsMandante p

-- Lista de times do campeonato, ordenado e sem repetições
times :: [Partida] -> [Time]
times ps = sort (nub(map mandante ps ++ map visitante ps)) -- nub retira os repetidos e sort ordena