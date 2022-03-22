{- |

= Introdução 
Nesta tarefa, o objetivo principal era implementar um bot eficaz que obedecia a determinados 
comportamentos, dependendo da condição a que está sujeito. Para a sua realização, apoiamo-nos
em raciocínios semelhantes aos da tarefa5.

= Objetivos
De modo a realizarmos um bot eficaz, começamos por pensar nas condições a que este podia estar sujeito.
Portanto, decidimos diversas estratégias para quando o bot se depara com uma parede, quando o pacman está em modo Mega, 
quando o fantasma mais próximo se encontra num determinado raio e quando existe comida pequena e grande em volta do bot.

Estratégia quando a próxima peça é parede : Para este caso, decidimos calcular as orientações em que não tem parede e, por
conseguinte, calculamos destas orientações a que se encontra mais longe do fantasma mais próximo.

Estratégia quando está em modo Mega : Dividimos este caso em duas possíveis movimentações: uma em que o pacman e está em
modo Mega e existe pelo menos um fantasma vivo; e o outra em que o pacman está em Mega mas não existem fantasmas vivos. Para
a primeira, decidimos aplicar a estratégia usada para quando a próxima peça é parede. Para a segunda, decidimos aplicar uma 
estratégia semelhante a esta, só que difere no facto de em vez de escolher a orientação mais longe do fantasma mais próximo, escolhe
a orientação mais perto deste fantasma.

Estratégia quando o fantasma mais próximo está dentro de um raio : Neste caso, definimos uma distância de 5 unidades para o raio e,
quando o fantasma mais próxima se encontra numa distância ao pacman inferior a esse valor, então aciona o mecanismo de começar a fugir
que corresponde, novamente, à estratégia utilizada para quando é parede.

Estratégia quando existem comidas em volta do pacman: Neste caso, dividimos em dois estratégias análogas uma para a food big e outra 
para a food little, dando prioridade à food big. Fizemos uma função que verifica se existe comida em volta, em caso afirmativo dirige-se 
para a comida. Em caso negativo, mantém a orientação.

= Discussão e conclusão
Em conclusão, pensamos que poderiamos ter melhorado e aperfeiçoado certos aspetos do bot. No entanto, achamos que conseguimos realizar  
uma estratégia boa e eficaz para o bot.

-}

module Tarefa6 where

import Types 
import Tarefa2
import Data.List

-- | Função principal que determina uma jogada segundo um state
bot :: Int -> State -> Maybe Play 
bot id s@(State m pls lvl) 
 | isWall s id = Just (Move id bestOriToRun)
 | (isMega (getPacman pls) && (checkGhostsAlive pls)) && not (insideTheRay rayAlive) = Just (Move id bestOriToChase)
 | (isMega (getPacman pls) && (checkGhostsAlive pls)) && insideTheRay rayAlive = Just (Move id bestOriToRun)
 | isMega (getPacman pls) && not (checkGhostsAlive pls) = Just (Move id bestOriToChase)
 | foodBigArea m (getPacmanCoords pls) = Just (goToFoodBig m id (getPacmanCoords pls) (getPlayerOrientation (getPacman pls)))
 | insideTheRay ray = Just (Move id bestOriToRun)
 | foodLittleArea m (getPacmanCoords pls) = Just (goToFoodLittle m id (getPacmanCoords pls) (getPlayerOrientation (getPacman pls)))
 | otherwise = Just (Move id (getPlayerOrientation (getPacman pls)))
             where bestOriToRun = bestOriToRunWithoutWall (oriWithoutWallsforBot m (getPlayerCoords (closestGhost (getPacman pls) (getGhosts s))) (fourCoords (getPacmanCoords pls)))
                   bestOriToChase = bestOriToChaseWithoutWall (oriWithoutWallsforBot m (getPlayerCoords (closestGhost (getPacman pls) (getGhosts s))) (fourCoords (getPacmanCoords pls)))
                   ray = (distanceToGhosts (getPacmanCoords pls) (ghostCoords (getGhosts s)))
                   rayAlive = (distanceToGhosts (getPacmanCoords pls) (ghostCoords (getGhostsAlive s)))

-- | Função que determina a lista das distâncias do pacman aos ghosts
distanceToGhosts :: Coords -> [Coords] -> [Float] 
distanceToGhosts _ [] = []
distanceToGhosts (x1,y1) ((x2,y2):xs) = (distance (x1,y1) (x2,y2)) : distanceToGhosts (x1,y1) xs

-- | Função que verifica se algum ghost está dentro do valor do raio
insideTheRay :: [Float] -> Bool
insideTheRay [] = False
insideTheRay (x:xs)
 | x < 7 = True
 | otherwise = insideTheRay xs

-- | Função que retorna uma play em direção à comida grande
goToFoodBig :: Maze -> Int -> Coords -> Orientation -> Play 
goToFoodBig m id (x,y) ori
 | getPiece m (orientationAux ori (x,y)) == (Food Big) = (Move id ori)
 | getPiece m (x+1,y) == (Food Big) = Move id D
 | getPiece m (x-1,y) == (Food Big) = Move id U
 | getPiece m (x,y+1) == (Food Big) = Move id R
 | getPiece m (x,y-1) == (Food Big) = Move id L

-- | Função que verifica se tem comida grande em volta do pacman
foodBigArea :: Maze -> Coords -> Bool
foodBigArea m (x,y)
 | getPiece m (x+1,y) == (Food Big) = True
 | getPiece m (x-1,y) == (Food Big) = True
 | getPiece m (x,y+1) == (Food Big) = True
 | getPiece m (x,y-1) == (Food Big) = True
 | otherwise = False

-- | Função que retorna uma play em direção à comida pequena
goToFoodLittle :: Maze -> Int -> Coords -> Orientation -> Play
goToFoodLittle m id (x,y) ori
 | getPiece m (orientationAux ori (x,y)) == (Food Little) = (Move id ori)
 | getPiece m (x+1,y) == (Food Little) = Move id D
 | getPiece m (x-1,y) == (Food Little) = Move id U
 | getPiece m (x,y+1) == (Food Little) = Move id R
 | getPiece m (x,y-1) == (Food Little) = Move id L

-- | Função que verifica se tem comida pequena em volta do pacman
foodLittleArea :: Maze -> Coords -> Bool
foodLittleArea m (x,y)
 | getPiece m (x+1,y) == (Food Little) = True
 | getPiece m (x-1,y) == (Food Little) = True
 | getPiece m (x,y+1) == (Food Little) = True
 | getPiece m (x,y-1) == (Food Little) = True
 | otherwise = False

-- | Função que determina o ghost mais proximo do pacman
closestGhost :: Player -> [Player] -> Player
closestGhost p l = head (sortOn  (distanceFromPlayers p) l)

-- | Função que determina a distancia entre 2 jogadores
distanceFromPlayers :: Player -> Player -> Float
distanceFromPlayers p1 p2 = let (a1,b1) = getPlayerCoords p1
                                (a2,b2) = getPlayerCoords p2 
                            in sqrt ((fromIntegral a1 - fromIntegral a2)^2 + (fromIntegral b1 - fromIntegral b2)^2)      

-- | Função que escolhe a melhor orientação para perseguir sem parede
bestOriToChaseWithoutWall :: [(Orientation,Float)] -> Orientation -- coords do closest ghost
bestOriToChaseWithoutWall l = fst ( head (sortOn (snd) l))

-- | Função que escolhe a melhor orientação para fugir sem parede
bestOriToRunWithoutWall :: [(Orientation,Float)] -> Orientation -- coords do closest ghost
bestOriToRunWithoutWall l = fst ( head (reverse (sortOn (snd) l)))

-- | Função que retira as orientações do bot que têm parede
oriWithoutWallsforBot :: Maze -> Coords -> ([Coords],[Orientation]) -> [(Orientation,Float)] -- usar fourCoords e coords do ghost
oriWithoutWallsforBot mz@(z:zs) gc ([],[]) = []
oriWithoutWallsforBot mz@(z:zs) gc ((x:xs),(y:ys))
 | or [getPiece mz x == Wall , x == ((div (length mz) 2),(length z)-1) , x == ((div (length mz) 2),0) , x == ((div (length mz) 2)-1,(length z)-1),x == ((div (length mz) 2)-1,0), x == ((div (length mz) 2)-1,div (length z) 2), x == ((div (length mz) 2)-1,1+(div (length z) 2)),x == ((div (length mz) 2)-1,(div (length z) 2)-1)] = oriWithoutWallsforBot mz gc (xs,ys)
 | otherwise = (y,distance gc x) : oriWithoutWallsforBot mz gc (xs,ys)

-- | Função que retira as orientações do fantasma que têm parede
oriWithoutWallsforGhost :: Maze -> Coords -> ([Coords],[Orientation]) -> [(Orientation,Float)] -- usar fourCoords e coords do ghost
oriWithoutWallsforGhost mz@(z:zs) gc ([],[]) = []
oriWithoutWallsforGhost mz@(z:zs) gc ((x:xs),(y:ys))
 | or [getPiece mz x == Wall , x == ((div (length mz) 2),(length z)-1) , x == ((div (length mz) 2),0) , x == ((div (length mz) 2)-1,(length z)-1),x == ((div (length mz) 2)-1,0)] = oriWithoutWallsforGhost mz gc (xs,ys)
 | otherwise = (y,distance gc x) : oriWithoutWallsforGhost mz gc (xs,ys)

-- | Função auxiliar que retira o segundo elemento de um tuplo com tres elementos
sndAux (a,b,c) = b

-- | Função que dá as coordenadas consoante a orientação
orientationAux :: Orientation -> Coords -> Coords
orientationAux U (x,y) = (x-1,y)
orientationAux D (x,y) = (x+1,y)
orientationAux R (x,y) = (x,y+1)
orientationAux L (x,y) = (x,y-1)

-- | Função que devolve a lista das quatro orientaçoes e das quatro coordenadas em volta de um jogador.
fourCoords :: Coords -> ([Coords],[Orientation])
fourCoords (x,y) = ([(x+1,y),(x-1,y),(x,y+1),(x,y-1)],[D,U,R,L])























