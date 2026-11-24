module Main (main) where

import Classificacao
import Leitura
import Dados

main :: IO ()
main = do
    case lerCampeonato dadosCampeonato of
        Left erro -> putStrLn ("Erro nos dados: " ++ mensagemErro erro)
        Right partidas -> do
            putStrLn "===== TABELA FINAL ====="
            putStr (tabelaFormatada (classificacao partidas))
            putStrLn "\n===== TABELA APOS A RODADA 3 ====="
            putStr (tabelaFormatada (classificacao (ateRodada 3 partidas)))
            putStrLn "\n===== ESTATISTICAS ====="
            putStrLn ("Maior goleada: " ++ maybe "nenhuma" formatarPartida (maiorGoleada partidas))
            putStrLn ("Media de gols: " ++ maybe "nenhuma" duasCasas (mediaGols partidas))
    putStrLn "\n===== ENTRADA COM ERROS ====="
    putStr (unlines (map mensagemErro (todosOsErros dadosComErros)))
