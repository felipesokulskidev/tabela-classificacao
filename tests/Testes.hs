-- testes do projeto
module Main (main) where

import Tipos -- funcoes da camada 1
import Dados -- campeonato de teste
import Classificacao -- funcoes da camada 2

-- compara o resultado obtido com o esperado
testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
    | obtido == esperado = putStrLn ("OK " ++ nome)
    | otherwise = putStrLn ("FALHOU " ++ nome ++ ": " ++ show obtido)

-- partida usada nos testes
jogo :: Partida
jogo = Partida 1 "Vasco" 7 1 "AthleticoPR"

-- linhas usadas no desempate
vasco :: Linha
vasco = linhaDoTime campeonatoTeste "Vasco"

flu :: Linha
flu = linhaDoTime campeonatoTeste "Fluminense"

main :: IO ()
main = do
    putStrLn "--- Camada 1 ---"

    -- resultado pro mandante, pro visitante e pra quem nao jogou
    testar "resultadoPara mandante" (resultadoPara "Vasco" jogo) (Just Vitoria)
    testar "resultadoPara visitante" (resultadoPara "AthleticoPR" jogo) (Just Derrota)
    testar "resultadoPara nao jogou" (resultadoPara "Flamengo" jogo) Nothing

    -- times sem repetir e em ordem alfabetica
    testar "times do campeonato" (times campeonatoTeste) ["AthleticoPR","Corinthians","Coritiba","Flamengo","Fluminense","Vasco"]

    
    putStrLn "--- Camada 2 ---"

    -- tabela final conferida a mao
    testar "classificacao final" (map time (classificacao campeonatoTeste)) ["Corinthians","Coritiba","Vasco","Fluminense","Flamengo","AthleticoPR"]

    -- vasco e flu tem 7 pontos e 2 vitorias, o saldo decide
    testar "desempate saldo" (map saldo [vasco, flu]) [5, 0]
    testar "desempate ordem" (filter (`elem` ["Vasco","Fluminense"]) (map time (classificacao campeonatoTeste))) ["Vasco","Fluminense"]

    -- tabela ate a rodada 3, diferente da final
    testar "classificacao ate rodada 3" (map time (classificacao (ateRodada 3 campeonatoTeste))) ["Corinthians","Vasco","Fluminense","Flamengo","AthleticoPR","Coritiba"]

    -- lista vazia da Nothing
    testar "maiorGoleada vazia" (maiorGoleada []) Nothing
    testar "mediaGols vazia" (mediaGols []) Nothing
    