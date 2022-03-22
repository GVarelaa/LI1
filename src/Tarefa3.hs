{- |

= Introdução
Esta tarefa consiste na criação de um função de compactação, ou seja, 
uma função que dado um labrinto, compacta-o transformando-o numa lista de Instruction. 

= Objetivos 
Os nossos objetivos para esta tarefa passaram por compactar o labirinto, utilizando 
os padrões descritos no enunciado.

= Discussão e conclusão
Em conclusão, pensamos que podiamos ter melhorado e aperfeiçoado a tarefa.

-}

-- | Este módulo tem como objetivo compactar um labirinto
module Tarefa3 where

import Types

-- | Esta função de teste recebe o labirinto a compactar e as instructions desse labirinto. Se o maze passado como argumento e a função portoCampeao (instructions) foram iguais, entao devolve True.
testDescompactar :: Maze -> Instructions -> Bool
testDescompactar m ins = m == instructionsToMaze (tiraRepeats ins ins)

-- | Função auxiliar de teste que nos dá uma lista formada pelo elemento "a" repetido "n" vezes
myreplicate :: Int -> a -> [a]
myreplicate n a | n > 0 = a: (myreplicate (n-1) a) 
                | otherwise = []

-- | Esta função auxiliar de teste converte as Instructions num corredor por extenso 
instructionToCorridor :: Instruction -> Corridor 
instructionToCorridor (Instruct []) = []
instructionToCorridor (Instruct ((x,y):t)) = myreplicate x y ++ instructionToCorridor (Instruct t)

-- | Esta função auxiliar de teste é basicamente a instructionToCorridor, porém substitui as Instructions pelos corredores em todos os corredores do labirinto portoCampeao (tiraRepeats) 
instructionsToMaze :: Instructions -> Maze 
instructionsToMaze [] = []
instructionsToMaze (h:t) = instructionToCorridor h : instructionsToMaze t

-- | Esta função auxiliar de teste, no fundo, vai receber duas instructions, mas ambas iguais. Uma das instructions vai servir para "guardar" a lista
tiraRepeats :: Instructions -> Instructions -> Instructions -- repeat index
tiraRepeats ins [] = []
tiraRepeats ins (Repeat x:xs) = ((!!) ins x) : tiraRepeats ins xs
tiraRepeats ins (x:xs) = x : tiraRepeats ins xs

-- | Função objetivo : 
--   função que tem como argumento um labirinto que pretendemos compactar e retorna esse labirinto compactado com o menor número de instruções possíveis
compactMaze :: Maze -> Instructions
compactMaze m = groupInstructions 0 (mazePatternInstruction m)

-- | Funções Principais :
-- Função que tem como argumento um corridor que pretendemos transformar numa lista do tipo (Int,Piece) com o menor número possível de elementos.
corridorPattern :: Corridor -> [(Int,Piece)]
corridorPattern [] = []
corridorPattern a@(x:xs) = (count a, x) : corridorPattern (subtraiLista (count a) a)

-- | Função que tem como argumento um corridor e devolve esse corridor no tipo Instruction já com o menor número possível de padrões horizontais
corridorPatternInstruction :: Corridor -> Instruction
corridorPatternInstruction [] = Instruct []
corridorPatternInstruction c = Instruct (corridorPattern c)

-- | Função que que tem como argumento uma labirinto e a cada corridor do labirinto é utilizada a função __corridorPatternInstruction__
mazePatternInstruction :: Maze -> Instructions
mazePatternInstruction [] = []
mazePatternInstruction (x:xs) = corridorPatternInstruction x : mazePatternInstruction xs

-- | Função que tem como argumento um inteiro que vai servir como acumulador, uma instruction e uma instructions, retornando, no caso de a instruction recebida estar repetida, uma instructions com repeat
groupInstructionsElem :: Int -> Instruction -> Instructions -> Instructions
groupInstructionsElem i y [] = [y]
groupInstructionsElem i y a@(x:xs)
 | elem y xs = replaceElem (tail (getIndexList 0 y a)) (Repeat i) (x:xs)
 | otherwise = (x:xs)

-- | Função que recebe como argumento um inteiro que vai servir como acumulador e uma insctructions, que vai retornar estas instructions num formato em que quando há corridores repetidos, substitui o corridor repetido por Repeat (index do corridor que repetiu)
groupInstructions:: Int -> Instructions -> Instructions
groupInstructions i [] = []
groupInstructions i (Repeat a : xs) = Repeat a : groupInstructions (i+1) xs
groupInstructions i (x:xs) 
 | elem x xs = groupInstructions i (groupInstructionsElem i x (x:xs))
 | otherwise = x : groupInstructions (i+1) xs

-- | Funções auxiliares : 
-- Função que recebe um elemento e uma lista de elementos e retorna o index da primeira aparição do elemento na lista
getIndex :: Eq a => a -> [a] -> Int
getIndex y [x] = 0
getIndex y (x:xs)
 | y == x = 0
 | otherwise = 1 + getIndex y xs

-- | Função que recebe um inteiro que vai servir como acumulador, um elemento, uma lista de elementos e retorna uma lista com os index's onde se econtra o elemento na lista
getIndexList :: Eq a => Int -> a -> [a] -> [Int] -- i = 0
getIndexList i y [] = []
getIndexList i y (x:xs)
 | y == x = i : getIndexList (i+1) y xs
 | otherwise = getIndexList (i+1) y xs

-- | Função que recebe uma lista de inteiro, que no contexto da Tarefa3 será a lista do indexs, o elemento que queremos dar replace na lista, uma lista de elementos e retorna a lista com o elemento colocado nos index's que introduzimos
replaceElem :: [Int] -> a -> [a] -> [a]
replaceElem [] a (y:ys) = (y:ys)
replaceElem (x:xs) a (y:ys) = replaceNElem x a (replaceElem xs a (y:ys))

-- | Função que recebe um inteiro que vai servir como acumulador, uma lista de elementos e retorna a lista de elementos, quando o acumulador chegar a zero
subtraiLista :: Int -> [a] -> [a]
subtraiLista y [] = []
subtraiLista y (x:xs)
 | y == 0 = (x:xs)
 | otherwise = subtraiLista (y-1) xs

-- | Função que recebe uma lista e conta a primeira aparição de elementos consecutivos iguais
count :: Eq a => [a] -> Int
count [] = 0
count [x] = 1
count (x:y:xs)
 | x == y = 1 + count (y:xs)
 | otherwise = 1
