module Main (main) where

import Classificacao
import Leitura
import Dados

-- a única função com IO: obtém os dados e imprime a tabela ou as mensagens de erro
main :: IO ()
main = do
    case lerCampeonato dadosCampeonato of
        Left erro -> putStrLn ("Erro nos dados: " ++ show erro)
        Right partidas -> do
            putStrLn "Tabela final:"
            putStr (tabelaFormatada (classificacao partidas))
            putStrLn "\nTabela apos a rodada 3:"
            putStr (tabelaFormatada (classificacao (ateRodada 3 partidas)))
            putStrLn "\nEstatisticas:"
            putStrLn ("Maior goleada: " ++ show (maiorGoleada partidas))
            putStrLn ("Media de gols: " ++ show (mediaGols partidas))
    putStrLn "\nEntrada com erros:"
    mapM_ print (todosOsErros dadosComErros)