-- | Este módulo serve da base para a tarefa1
module Generator where

import System.Random
import Types

-- | Given a seed returns a list of n integer randomly generated
--
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,9) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- | Converssta list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converts an integer number into a Piece
-- 3 <=> Food Granfre
-- 0 <= n < 7 <=> Food Little
-- 7 < n <= 9 <=> Wall
--
convertePiece :: Int -> Piece
convertePiece x 
    | x == 3 = Food Big
    | x> 7 && x<=9 = Wall
    | otherwise = Food Little

-- | Converts a Corredor to a string
--
printCorridor :: Corridor -> String
printCorridor []= "\n"
printCorridor (x:xs) = show x ++ printCorridor xs


-- | Converts a Labirinto to a string
--
printMaze' :: Maze -> String
printMaze' [] = ""
printMaze' (x:xs) = printCorridor x ++  printMaze' xs

-- | Prints the maze on the screen
mazeToString :: Maze -> IO ()
mazeToString x = putStrLn (printMaze' x)

-- | Converts a list of integers into a Corredor
--
converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (x:xs) = convertePiece x : converteCorredor xs


-- | Converts a list of lists of integers into a Labirinto
--
converteLabirinto :: [[Int]] -> Maze
converteLabirinto [] = []
converteLabirinto (x:xs) = converteCorredor x : converteLabirinto xs

-- | Função que gera um labirinto
geraLabirinto :: Int -> Int -> Int -> Maze
geraLabirinto x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in converteLabirinto $ subLista x random_nrs

