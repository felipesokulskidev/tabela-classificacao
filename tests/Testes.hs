-- testes do projeto
module Main (main) where

import Tipos -- funcoes da camada 1
import Dados -- campeonato de teste
import Classificacao -- funcoes da camada 2
import Leitura -- funcoes da camada 3

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
    
    
    putStrLn "--- Camada 3 ---"

    -- separar quebra a linha nos ;
    testar "separar" (separar ';' "1;Vasco;2;1;Flamengo") ["1","Vasco","2","1","Flamengo"]

    -- linha certa vira partida
    testar "lerPartida valida" (lerPartida 1 "1;Vasco;7;1;AthleticoPR") (Right jogo)

    -- um teste pra cada erro
    testar "erro CamposInvalidos" (lerPartida 3 "1;Vasco;2;1") (Left (CamposInvalidos 3))
    testar "erro NumeroInvalido" (lerPartida 2 "1;Vasco;dois;1;Flamengo") (Left (NumeroInvalido 2))
    testar "erro PlacarNegativo" (lerPartida 4 "2;Vasco;-1;0;Coritiba") (Left (PlacarNegativo 4))
    testar "erro MesmoTime" (lerPartida 5 "3;Flamengo;1;1;Flamengo") (Left (MesmoTime 5))
    testar "erro TimeRepetidoNaRodada" (lerCampeonato (unlines ["4;Vasco;1;0;Coritiba","4;Vasco;2;2;Fluminense"])) (Left (TimeRepetidoNaRodada 4 "Vasco"))

    -- texto certo da o mesmo campeonato da lista
    testar "lerCampeonato valido" (lerCampeonato dadosCampeonato) (Right campeonatoTeste)

    -- lerCampeonato para no primeiro erro
    testar "lerCampeonato para no 1o erro" (lerCampeonato dadosComErros) (Left (NumeroInvalido 2))

    -- todosOsErros pega todos
    testar "todosOsErros" (todosOsErros dadosComErros) [NumeroInvalido 2, CamposInvalidos 3, PlacarNegativo 4, MesmoTime 5, TimeRepetidoNaRodada 4 "Vasco"]

    -- cabecalho + 6 times
    testar "tabelaFormatada linhas" (length (lines (tabelaFormatada (classificacao campeonatoTeste)))) 7

    putStrLn "--- Casos de borda ---"

    -- campeonato vazio nao quebra
    testar "lerCampeonato vazio" (lerCampeonato "") (Right [])
    testar "so comentario e linha vazia" (lerCampeonato "# nada aqui\n\n") (Right [])
    testar "classificacao vazia" (classificacao []) []
    testar "aproveitamento sem jogos" (aproveitamento (Linha "Ninguem" 0 0 0 0 0 0)) 0