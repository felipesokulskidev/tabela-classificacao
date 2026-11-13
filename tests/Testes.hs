-- testes do projeto
module Main (main) where

import Tipos -- funcoes da camada 1
import Dados -- campeonato de teste

-- compara o resultado obtido com o esperado
testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
    | obtido == esperado = putStrLn ("OK " ++ nome)
    | otherwise = putStrLn ("FALHOU " ++ nome ++ ": " ++ show obtido)

-- partida usada nos testes
jogo :: Partida
jogo = Partida 1 "Vasco" 7 1 "AthleticoPR"

main :: IO ()
main = do
    putStrLn "--- Camada 1 ---"

    -- resultado pro mandante, pro visitante e pra quem nao jogou
    testar "resultadoPara mandante" (resultadoPara "Vasco" jogo) (Just Vitoria)
    testar "resultadoPara visitante" (resultadoPara "AthleticoPR" jogo) (Just Derrota)
    testar "resultadoPara nao jogou" (resultadoPara "Flamengo" jogo) Nothing

    -- times sem repetir e em ordem alfabetica
    testar "times do campeonato" (times campeonatoTeste) ["AthleticoPR","Corinthians","Coritiba","Flamengo","Fluminense","Vasco"]