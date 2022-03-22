{- |

= Introdução

Na tarefa1, foi nos pedido para criar a função "generateMaze" que gera Labirintos 
consoante a dimensão que pretendemos.

= Objetivos
Os nossos objetivos para esta tarefa passaram por construir labirintos com a casa 
dos fantasmas no centro, da maneira mais "fácil" possível e implementar testes. 
Para tal, começamos por encarar o desafio de uma forma gradual, partindo da construção de funções 
que utilizam o tipo Piece, para funções que utilizam o tipo Corridor e, finalmente, 
para funções que utilizam o tipo Maze.

= Discussão e conclusão
Em conclusão, penso que conseguimos construir os labirintos que cumprem todas as instruções pedidas 
de uma maneira efetiva. Poderiamos, talvez, melhorar e desensolver mais os testes.

-}

-- | Este módulo tem como objetivo construir um labirinto em função das instruções dadas
module Tarefa1 where

import Types
import System.Random

-- | Testes :
-- | Função que testa se um labirinto tem a medida vertical correta
testVerticalSize :: Int -> Int -> Int -> Bool
testVerticalSize x y s = length (generateMaze x y s) == y

-- | Função que testa se os corridores de um labirinto têm todos o mesmo comprimento que tem de ser igual ao input de x
testHorizontalSize :: Maze -> Int -> Bool
testHorizontalSize [] y = True
testHorizontalSize (x:xs) y = length x == y && testHorizontalSize xs y

-- | Função usada no meio da tarefa 1, porque no final já nao pode ser usada devido aos túneis
testWallOnSide :: Maze -> Bool
testWallOnSide [] = True
testWallOnSide (x:xs) = testCorridorWall x && testWallOnSide xs
          where testCorridorWall :: Corridor -> Bool
                testCorridorWall (x:xs) = x == Wall && last (x:xs) == Wall

-- | Função que testa se um labirinto tem os túneis bem colocados tanto para quando a altura é impar ou par
testTunnel :: Maze -> Bool
testTunnel m@(x:xs)
 | odd (length m) = testCorridorEmpty ((!!) m (div (length m) 2))
 | otherwise = testCorridorEmpty ((!!) m (div (length m) 2)) && testCorridorEmpty ((!!) m ((div (length m) 2)-1))
         where testCorridorEmpty :: Corridor -> Bool
               testCorridorEmpty (x:xs) = x == Empty && last (x:xs) == Empty

-- | Função que testa se a casa dos fantasmas e os emptys em volta estão bem colocados no labirinto
testGhostHouse :: Maze -> Bool
testGhostHouse m@(x:xs) 
 | even (length m) && even (length x) = retiraMaze (div (length x - 10) 2) ([(!!) m a] ++ [(!!) m b] ++ [(!!) m c] ++ [(!!) m d] ++ [(!!) m e]) == [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 
 | even (length m) && odd (length x) = retiraMaze (div (length x - 10) 2) ([(!!) m a] ++ [(!!) m b] ++ [(!!) m c] ++ [(!!) m d] ++ [(!!) m e]) == [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 
 | odd (length m) && even (length x) = retiraMaze (div (length x - 10) 2) ([(!!) m b] ++ [(!!) m c] ++ [(!!) m d] ++ [(!!) m e] ++ [(!!) m f]) == [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 
 | odd (length m) && odd (length x) = retiraMaze (div (length x - 10) 2) ([(!!) m b] ++ [(!!) m c] ++ [(!!) m d] ++ [(!!) m e] ++ [(!!) m f]) == [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 
         where a = (div (length m) 2) - 3
               b = (div (length m) 2) - 2
               c = (div (length m) 2) - 1
               d = (div (length m) 2)
               e = (div (length m) 2) + 1
               f = (div (length m) 2) + 2

-- | Função auxiliar para a testGhostHouse
retiraMaze :: Int -> Maze -> Maze
retiraMaze y [] = []
retiraMaze 0 (x:xs) = (x:xs)
retiraMaze y (x:xs) = retiraCorridor y x : retiraMaze y xs

-- | Função auxiliar para a retiraMaze
retiraCorridor :: Int -> Corridor -> Corridor
retiraCorridor 0 (x:xs) = (x:xs)
retiraCorridor y (x:xs) = retiraCorridor (y-1) (init xs)

-- | Função principal que gera o labirinto e implementa as funções secundárias
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s = (devolveWalls (x)) : colocarTunel (colocarCasaFantasmas (geraLabirinto x (y-2) s)) ++ [devolveWalls (x)]        

-- | Função que dado um inteiro converte esse inteiro num corredor com walls 
devolveWalls :: Int -> Corridor
devolveWalls 0 = []
devolveWalls x = Wall : devolveWalls (x-1)
 
-- | Função que mete a casa dos fantasmas no meio de um labirinto, que difere quando o labirinto tem altura par ou ímpar
colocarCasaFantasmas :: Maze -> Maze 
colocarCasaFantasmas [] = []                   
colocarCasaFantasmas (x:xs) = let meio = getMiddle (x:xs)
                              in if odd (length (x:xs)) then (take ((length (x:xs))-meio-3) (x:xs)) ++ [replaceInList' ((div (length x-8) 2)-1) (length (getFirstCorridor (constroiCasa (length x))) + 2) ((!!) (x:xs) (meio-2))] ++ meteCasaNosCorridores (selecionaCorridoresMeio 0 (x:xs)) (constroiCasa (length x)) ++ [replaceInList' ((div (length x-8) 2)-1) (length (getFirstCorridor (constroiCasa (length x))) + 2) ((!!) (x:xs) (meio+1))] ++ (drop (meio + 3) (x:xs))
                                                          else (take ((length (x:xs))-meio-3) (x:xs)) ++ [replaceInList' ((div (length x-8) 2)-1) (length (getFirstCorridor (constroiCasa (length x))) + 2) ((!!) (x:xs) (meio-3))] ++ meteCasaNosCorridores (selecionaCorridoresMeio 0 (x:xs)) (constroiCasa (length x)) ++ [replaceInList' ((div (length x-8) 2)-1) (length (getFirstCorridor (constroiCasa (length x))) + 2) ((!!) (x:xs) (meio+1))] ++ (drop (meio + 2) (x:xs))

-- | Função responsável por colocar os túneis no labirinto, que difere dependendo da paridade da altura do labirinto
colocarTunel :: Maze -> Maze
colocarTunel x = let meio = getMiddle x 
                 in if odd (length x) then replaceInCorridor (colocarWall x) meio Empty
                                        else replaceInCorridor (replaceInCorridor (colocarWall x) (meio-1) Empty) meio Empty

-- | Função que vai a um labirinto e coloca uma parede no início e no fim de cada corredor
colocarWall :: Maze -> Maze
colocarWall [] = []
colocarWall (x:xs) = replaceInList (length x - 1) Wall (replaceInList 0 Wall x) : colocarWall xs

-- | Função que escolhe um corredor e coloca uma peça no início e no fim de cada corredor 
replaceInCorridor :: Maze -> Int -> Piece -> Maze 
replaceInCorridor [] y p = []
replaceInCorridor (x:xs) 0 p = replaceInList (length x - 1) p (replaceInList 0 p x) : xs
replaceInCorridor (x:xs) y p = x : replaceInCorridor xs (y-1) p

-- | Função que escolhe a posição de uma peça no corredor e substitui por outra peça
replaceInList :: Int -> Piece -> Corridor -> Corridor
replaceInList n p [] = [] -- substitui numa lista 0 indice i por uma peça       
replaceInList 0 p (x:xs) = p : xs
replaceInList n p (x:xs) = x : replaceInList (n-1) p xs

-- | Função que substitui as peças por uma peça ao longo do corredor 
replaceInList' :: Int -> Int -> Corridor -> Corridor
replaceInList' z 0 c = c 
replaceInList' z y [] = []
replaceInList' z y (x:xs) = replaceInList z Empty (replaceInList' (z+1) (y-1) (x:xs))

-- | Função que seleciona os corredores centrais do labirinto
selecionaCorridoresMeio :: Int -> Maze -> Maze
selecionaCorridoresMeio y [] = []
selecionaCorridoresMeio y l = let meio = getMiddle l
                              in if odd (length l) then [(!!) l (meio-1)] ++ [(!!) l meio] ++ [(!!) l (meio+1)]   
                                                    else [(!!) l (meio-2)] ++ [(!!) l (meio-1)] ++ [(!!) l meio]

-- | Função que coloca a casa dos fantasmas nos corredores centrais
meteCasaNosCorridores :: Maze -> Maze -> Maze
meteCasaNosCorridores [] [] = []
meteCasaNosCorridores (x:xs) (c:cs) = (take ((div (length x - length c) 2)-1) x ++ [Empty] ++ c ++ [Empty] ++ drop (length x - (div (length x - length c) 2)+1) x) : meteCasaNosCorridores xs cs    -- take + casa + drop = length x

-- | Função que constrói a casa de fantasmas
constroiCasa :: Int -> Maze
constroiCasa x | even x = [[Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall],[Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]
               | otherwise = [[Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall],[Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall],[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

-- | Função que pega no primeiro corredor de um labirinto
getFirstCorridor :: Maze -> Corridor
getFirstCorridor (x:xs) = x

-- | Given a seed returns a list of n integer randomly generated
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,9) gen -- takes the first n elements from an infinite series of random numbers between 0-9

-- | Given a seed returns an integer randomly generated
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed

-- | Converssta list into a list of list of size n
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)

-- | Converts an integer number into a Piece
-- 3 <=> Food Granfre
-- 0 <= n < 7 <=> Food Little
-- 7 < n <= 9 <=> Wall
convertePiece :: Int -> Piece
convertePiece x 
    | x == 3 = Food Big
    | x> 7 && x<=9 = Wall
    | otherwise = Food Little

-- | Converts a Corredor to a string
printCorridor :: Corridor -> String
printCorridor []= "\n"
printCorridor (x:xs) = show x ++ printCorridor xs

-- | Converts a Labirinto to a string
printMaze' :: Maze -> String
printMaze' [] = ""
printMaze' (x:xs) = printCorridor x ++  printMaze' xs

-- | Prints the maze on the screen
mazeToString :: Maze -> IO ()
mazeToString x = putStrLn (printMaze' x)

-- | Converts a list of integers into a Corredor
converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (x:xs) = convertePiece x : converteCorredor xs

-- | Converts a list of lists of integers into a Labirinto
converteLabirinto :: [[Int]] -> Maze
converteLabirinto [] = []
converteLabirinto (x:xs) = converteCorredor x : converteLabirinto xs

-- | Função que gera um labirinto
geraLabirinto :: Int -> Int -> Int -> Maze
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in converteLabirinto $ subLista x random_nrs

