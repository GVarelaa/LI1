{- |

= Introdução 
Esta tarefa consiste na criação da função "play" que implementa todas as situações
possíveis do jogo e cria uma jogada, tanto para o pacman como para os fantasmas.

= Objetivos
Neste tarefa, os nossos objetivos passaram por criar um "data Action" que organiza 
todas as ações dos pacman, pretendemos criar uma função que descreve o movimento e 
implementar testes eficazes.
Começamos, então, por criar a função "coordsAfterPlay" que trata do movimento de cada
jogada. Depois, criamos o data "Action", ao qual criamos a função "action" que associa
este data e reproduz uma ação.

= Discussão e conclusão
Em conclusão, pensamos que poderiamos ter feito esta tarefa de uma maneira menos "robusta"
e mais organizada, inclusivamente os testes poderiam ser mais complexos e eficazes. 
Contudo, no final, a função play cumpre na íntegra o que foi exigido.

-}

-- | Este módulo tem como objetivo construir um estado em função de uma jogada
module Tarefa2 where

import Types 

-- | Testes : 
-- | Primeiro temos os exemplos de labirintos gerados para os nossos testes
sampleMaze = [[Wall,Wall,Wall,Wall,Wall],[Wall,Empty,PacPlayer (Ghost (GhoState (0,(1,1),0,R,0,0) Alive)),Empty,Wall],[Wall,Food Big,Empty,Food Little,Wall],[Wall,Empty,Empty,Empty,Wall],[Wall,Wall,Wall,Wall,Wall]]
sampleMazeWall = [[Wall,Wall,Wall],[Wall,Empty,Wall],[Wall,Wall,Wall]]
sampleMazeTunnel = [[Wall,Wall,Wall,Wall,Wall,Wall],[Wall,Empty,Empty,Empty,Empty,Wall],[Empty,Empty,Empty,Empty,Empty,Empty],[Wall,Empty,Empty,Empty,Empty,Wall],[Wall,Wall,Wall,Wall,Wall,Wall]]

-- | Função que testa se o Pacman realmente se move quando dada uma jogada cuja orientação coincida com a mesma do Pacman
testMovement :: Play -> Bool
testMovement a@(Move id R) = let state = State sampleMaze [Pacman (PacState (0,(2,2),1,R,0,3) 0 Open Normal)] 1 
                             in getPlayerCoords (getFirstPlayer (play a state)) == (2,3)
testMovement a@(Move id L) = let state = State sampleMaze [Pacman (PacState (0,(2,2),1,L,0,3) 0 Open Normal)] 1 
                             in getPlayerCoords (getFirstPlayer (play a state)) == (2,1)
testMovement a@(Move id U) = let state = State sampleMaze [Pacman (PacState (0,(2,2),1,U,0,3) 0 Open Normal)] 1 
                             in getPlayerCoords (getFirstPlayer (play a state)) == (1,2)
testMovement a@(Move id D) = let state = State sampleMaze [Pacman (PacState (0,(2,2),1,D,0,3) 0 Open Normal)] 1 
                             in getPlayerCoords (getFirstPlayer (play a state)) == (3,2)

-- | Função teste onde se verifica que o Pacman não se desloca para as coordenadas onde se encontra uma parede
testMovementWall :: Play -> Bool
testMovementWall a@(Move id R) = let state = State sampleMazeWall [Pacman (PacState (0,(1,1),1,R,0,3) 0 Open Normal)] 1 
                           in getPlayerCoords (getFirstPlayer (play a state)) == (1,1)
testMovementWall a@(Move id L) = let state = State sampleMazeWall [Pacman (PacState (0,(1,1),1,L,0,3) 0 Open Normal)] 1 
                           in getPlayerCoords (getFirstPlayer (play a state)) == (1,1)
testMovementWall a@(Move id U) = let state = State sampleMazeWall [Pacman (PacState (0,(1,1),1,U,0,3) 0 Open Normal)] 1 
                           in getPlayerCoords (getFirstPlayer (play a state)) == (1,1)
testMovementWall a@(Move id D) = let state = State sampleMazeWall [Pacman (PacState (0,(1,1),1,D,0,3) 0 Open Normal)] 1 
                           in getPlayerCoords (getFirstPlayer (play a state)) == (1,1)                                                      

-- | Função teste onde se verifica o que acontece quando o Pacman come comida (pequena ou grande) ou fantasmas
testEat :: Piece -> Bool
testEat (Food Little) = let state = play (Move 0 R) (State sampleMaze [Pacman (PacState (0,(2,2),1,R,0,3) 0 Open Normal)] 1)
                        in getPlayerPoints (getFirstPlayer state) == 1
testEat (Food Big) = let state = play (Move 0 L) (State sampleMaze [Pacman (PacState (0,(2,2),1,L,0,3) 0 Open Normal)] 1)
                     in getPlayerPoints (getFirstPlayer state) == 5 && getPacmanMode (getFirstPlayer state) == Mega
testEat (PacPlayer (Ghost (GhoState _ Alive))) = let state = play (Move 0 U) (State sampleMaze [Pacman (PacState (0,(2,2),1,U,0,3) 0 Open Normal)] 1)
                                                 in getPlayerLives (getFirstPlayer state) == 2
testEat (PacPlayer (Ghost (GhoState _ Dead))) = let state = play (Move 0 U) (State sampleMaze [Pacman (PacState (0,(2,2),1,U,0,3) 0 Open Normal)] 1)
                                                in getPlayerPoints (getFirstPlayer state) == 10 

-- | Função que verifica as coordenadas do Pacman após ter usado o túnel
testTunnel2 :: Bool            
testTunnel2 = let state = play (Move 0 L) (State sampleMazeTunnel [Pacman (PacState (0,(2,0),1,L,0,3) 0 Open Normal)] 1)
             in getPlayerCoords (getFirstPlayer state) == (2,5)

-- | Função auxiliar usada para código dos testes. A função recebe um estado e retorna o primeiro jogador da lista de jogadores desse estado
getFirstPlayer :: State -> Player 
getFirstPlayer (State m (x:xs) lvl) = x 

-- | Função principal que dada uma jogada e um estado determina o proximo estado em função dessa jogada
play :: Play -> State -> State 
play a@(Move id y) (State m (x:xs) l) 
 | getPacmanMode (getPacman (x:xs)) == Dying             = State m (x:xs) l
 | isPacman s && (getTimeMega (x:xs) > 0)                = State rpiece (tpGhost playersmap nextpiecepm nextpiecem (actionList nextactionpm nextactionm (changeMouth id applyPac))) l
 | isPacman s && ((getTimeMega (x:xs) == 0) && isMega s) = State rpiece (checkMega (updateToNormal id (tpGhost playersmap nextpiecepm nextpiecem (actionList nextactionpm nextactionm (changeMouth id applyPac))))) l
 | isPacman s                                            = State rpiece (checkMega (tpGhost playersmap nextpiecepm nextpiecem (actionList nextactionpm nextactionm (changeMouth id applyPac)))) l
 | not (isPacman s) && (getTimeMega (x:xs) == 0)         = State m (checkMega (updateToNormal (getPacID (x:xs)) applyGhost)) l 
 | not (isPacman s)                                      = State m applyGhost l 
            where rpiece       = removePiece m (nextPosition playersmap a (getPlayerState s) ) (nextpiecepm) s
                  s            = selectCertainPlayer id (x:xs) 
                  nextactionpm = determinateAction (nextpiecepm)
                  nextactionm  = determinateAction (nextpiecem)
                  playersmap   = placePlayersOnMap (x:xs) m
                  applyGhost   = applyMoveForGhost m a id (x:xs) (x:xs)
                  applyPac     = applyMoveForPacman m a id action (x:xs) (x:xs)
                  nextpiecepm  = selectCertainNPiece playersmap a (x:xs)
                  nextpiecem   = selectCertainNPiece m a (x:xs) 

-- | Função que verifica se o ID dado na jogada corresponde ao ID do jogador e, em caso afirmativo, aplica a função action a esse jogador.
applyMoveForPacman :: Maze -> Play -> Int -> (Action -> Action -> Player -> Player) -> [Player] -> [Player] -> [Player] 
applyMoveForPacman m play id f [] pls = []
applyMoveForPacman m play id f (x:xs) pls
 | id == getPlayerID x = f a b (fst (coordsAfterPlay playersmap play x npiece)) : xs
 | otherwise = x : applyMoveForPacman m play id f xs pls
            where a          = determinateAction (nextPiece playersmap play x)
                  b          = determinateAction (nextPiece m play x)
                  npiece     = nextPiece playersmap play x
                  playersmap = placePlayersOnMap pls m

applyMoveForGhost :: Maze -> Play -> Int -> [Player] -> [Player] -> [Player]
applyMoveForGhost m play id [] pls = []
applyMoveForGhost m play id (x:xs) pls
 | ((id == getPlayerID x) && ((nextPiece playersmap play x) == PacPlayer (getPacman pls))) && (getGhostMode x) == Alive = pacmanLoseLife ((fst (coordsAfterPlay playersmap play x npiece)) : xs)
 | ((id == getPlayerID x) && ((nextPiece playersmap play x) == PacPlayer (getPacman pls))) && (getGhostMode x) == Dead  = pacmanEatPoints (tpGhostSingle m (fst (coordsAfterPlay playersmap play x npiece)) : xs)
 | id == getPlayerID x = fst (coordsAfterPlay playersmap play x npiece) : xs
 | otherwise = x : applyMoveForGhost m play id xs pls
            where npiece     = nextPiece playersmap play x
                  playersmap = placePlayersOnMap pls m

-- | Função que verifica a next piece de um jogador é parede
isWall :: State -> Int -> Bool
isWall (State m pls lvl) id = getPiece m (guessCords (selectCertainPlayer id pls)) == Wall

-- | Função que dá as coordenadas segundo a orientação do jogador atual
guessCords :: Player -> Coords
guessCords p@(Pacman (PacState (x,(x1,y1),z,t,h,l) q c d))
 | getPlayerOrientation p == R = (x1,y1+1)
 | getPlayerOrientation p == L = (x1,y1-1)
 | getPlayerOrientation p == U = (x1-1,y1)
 | getPlayerOrientation p == D = (x1+1,y1)
guessCords g@(Ghost (GhoState (x,(x1,y1),z,t,h,l) q))
 | getPlayerOrientation g == R = (x1,y1+1)
 | getPlayerOrientation g == L = (x1,y1-1)
 | getPlayerOrientation g == U = (x1-1,y1)
 | getPlayerOrientation g == D = (x1+1,y1)

-- | Função responsável pelo pacman abrir/fechar a boca
updateMouth :: Player -> Player
updateMouth (Pacman (PacState ps tm Open pm)) = Pacman (PacState ps tm Closed pm)
updateMouth (Pacman (PacState ps tm Closed pm)) = Pacman (PacState ps tm Open pm)

-- | Função responsável pelo pacman abrir/fechar a boca
changeMouth :: Int -> [Player] -> [Player]              
changeMouth id [] = []
changeMouth id (x:xs) 
 | id == getPlayerID x = updateMouth x : xs
 | otherwise = x : changeMouth id xs

-- | Função que muda o estado do pacman para normal
playerToNormal :: Player -> Player
playerToNormal (Pacman (PacState ps tm m pm)) = Pacman (PacState ps tm m Normal)

-- | Função responsavel por transformar o pacman em normal numa lista de jogadores
updateToNormal :: Int -> [Player] -> [Player]
updateToNormal id [] = []
updateToNormal id (x:xs) 
 | id == getPlayerID x = playerToNormal x : xs
 | otherwise = x : updateToNormal id xs

-- | Função que coloca os ghosts vivos
updateGhostToNormal :: [Player] -> [Player]
updateGhostToNormal [] = []
updateGhostToNormal (Ghost (GhoState (a,(x,y),b,ori,c,d) gm):xs) = (Ghost (GhoState (a,(x,y),1,ori,c,d) Alive)) : updateGhostToNormal xs
updateGhostToNormal (x:xs) = x : updateGhostToNormal xs

-- | Função que verifica se existem jogadores mega
checkMegaPlayers :: [Player] -> Bool
checkMegaPlayers [] = False
checkMegaPlayers (x:xs)
 | isMega x = True
 | otherwise = False || checkMegaPlayers xs

-- | Função responsável por verificar a existencia de um jogador mega e colocar os fantasmas vivos 
checkMega :: [Player] -> [Player]
checkMega ps
 | checkMegaPlayers ps = ps
 | otherwise = updateGhostToNormal ps

-- | Função, utilizada na função de movimentação dos fantasmas, que os coloca na casa
tpGhostSingle :: Maze -> Player -> Player -- copiar do tpGhost e verificar se é par ou impar; acabar a funçao play pro ghost; fazer o caso em que o pacman está Dead (ig); 
tpGhostSingle m@(z:zs) (Ghost (GhoState (a,(x1,y1),b,ori,c,d) gm)) 
 | even (length m) = Ghost (GhoState (a,((getMiddle m)-1,div (length z) 2),(b*2),U,c,d) Alive ) 
 | odd (length m) = Ghost (GhoState (a,(getMiddle m,div (length z) 2),(b*2),U,c,d) Alive)

-- | Função, utilizada na função de movimentação dos fantamas, que adiciona 10 pontos ao pacman ao comer um fantasma
pacmanEatPoints :: [Player] -> [Player]
pacmanEatPoints [] = []
pacmanEatPoints (Pacman (PacState (x,y,z,t,h,l) q c d ):xs) = Pacman (PacState (x,y,z,t,h+10,l) q c d ) : xs
pacmanEatPoints (x:xs) = x : pacmanLoseLife xs

-- | Função, utiliza na função de movimentação dos fantasmas, que reduz a vida do pacman em uma unidade
pacmanLoseLife :: [Player] -> [Player]
pacmanLoseLife [] = []
pacmanLoseLife (Pacman (PacState (x,y,z,t,h,l) q c d ):xs)  
 | l == 0 = Pacman (PacState (x,y,z,t,h,0) q c Dying ) : xs
 | otherwise = Pacman (PacState (x,y,z,t,h,l-1) q c d ) : xs
pacmanLoseLife (x:xs) = x : pacmanLoseLife xs

-- | Função que seleciona de uma lista de jogadores, o jogador com o ID fornecido na jogada
selectCertainPlayer :: Int -> [Player] -> Player 
selectCertainPlayer id (x:xs)
 | id == getPlayerID x = x
 | otherwise = selectCertainPlayer id xs

-- | Função que determina a próxima peça em relação ao jogador com o ID escolhido na jogada
selectCertainNPiece :: Maze -> Play -> [Player] -> Piece 
selectCertainNPiece m p@(Move id y) (x:xs) = nextPiece m p (selectCertainPlayer id (x:xs))

-- | Função que determina a próxima posição de um jogador dada uma jogada
nextPosition :: Maze -> Play -> PlayerState -> Coords 
nextPosition (z:zs) (Move id L) (a,(x,0),b,L,c,d) = (x,0)
nextPosition (z:zs) (Move id L) (a,(x,y),b,L,c,d) = (x,y-1)
nextPosition (z:zs) (Move id R) (a,(x,y),b,R,c,d) 
 | y == (length z)-1= (x,(length z) -1)
 | y /= (length z)-1= (x,y+1)
nextPosition (z:zs) (Move id U) (a,(x,y),b,U,c,d)  = (x-1,y)
nextPosition (z:zs) (Move id D) (a,(x,y),b,D,c,d)  = (x+1,y)
nextPosition (z:zs) (Move id R) (a,(x,y),b,L,c,d)  = (x,y) 
nextPosition (z:zs) (Move id R) (a,(x,y),b,U,c,d)  = (x,y) 
nextPosition (z:zs) (Move id R) (a,(x,y),b,D,c,d)  = (x,y)
nextPosition (z:zs) (Move id L) (a,(x,y),b,R,c,d)  = (x,y) 
nextPosition (z:zs) (Move id L) (a,(x,y),b,D,c,d)  = (x,y) 
nextPosition (z:zs) (Move id L) (a,(x,y),b,U,c,d)  = (x,y) 
nextPosition (z:zs) (Move id U) (a,(x,y),b,R,c,d)  = (x,y) 
nextPosition (z:zs) (Move id U) (a,(x,y),b,D,c,d)  = (x,y) 
nextPosition (z:zs) (Move id U) (a,(x,y),b,L,c,d)  = (x,y) 
nextPosition (z:zs) (Move id D) (a,(x,y),b,R,c,d)  = (x,y) 
nextPosition (z:zs) (Move id D) (a,(x,y),b,L,c,d)  = (x,y) 
nextPosition (z:zs) (Move id D) (a,(x,y),b,U,c,d)  = (x,y) 
nextPosition (z:zs) (Move id L) (a,(x,y),b,Null,c,d) = (x,y-1)
nextPosition (z:zs) (Move id R) (a,(x,y),b,Null,c,d) = (x,y+1)
nextPosition (z:zs) (Move id U) (a,(x,y),b,Null,c,d) = (x-1,y)
nextPosition (z:zs) (Move id D) (a,(x,y),b,Null,c,d) = (x+1,y)

-- | Função que dado um labirinto e umas coordenadas, vai a essas coordenadas e seleciona a peça nessa posição
getPiece :: Maze -> Coords -> Piece 
getPiece (a:as) (x,y) = (!!) ((!!) (a:as) (x)) (y) -- saber peça da prox posicao (getPiece (nextPosition p psatual c))

-- | Função que seleciona a peça na proxima posição de um jogador, ou seja, a próxima peça, em função de uma jogada
nextPiece :: Maze -> Play -> Player -> Piece 
nextPiece m play player = getPiece m (nextPosition m play (getPlayerState player) )

-- | Função que remove do labirinto certas peças nas coordenadas dadas
removePiece :: Maze -> Coords -> Piece -> Player -> Maze
removePiece m (x,y) piece (Pacman (PacState _ _ _ Dying)) = m
removePiece m (x,y) (Food Little) _ = replaceElemInMaze (x,y) Empty m
removePiece m (x,y) (Food Big) _ = replaceElemInMaze (x,y) Empty m
removePiece m (x,y) (PacPlayer (Ghost (GhoState _ Dead))) _ = replaceElemInMaze (x,y) Empty m
removePiece m (x,y) (PacPlayer (Pacman (PacState  _ _ _ _))) _ = replaceElemInMaze (x,y) Empty m
removePiece m (x,y) _ _ = m

-- | Função que vai uma peça ghost e retorna o id da peça
getPieceGhostID :: Piece -> Int 
getPieceGhostID (PacPlayer (Ghost (GhoState (a,(x,y),b,o,c,d) gm))) = a

-- | Função que faz com que o fantasma morto apareça vivo na casa dos fantasmas. Esta função recebe duas peças, porque uma vai ser a próxima peça do labirinto com os jogadores e a outra vai ser a próxima peça do labirinto sem os jogadores. Utilizamos esta estratégia para os casos em que o pacman come um fantasma morto e por baixo dele está presente uma comida.
tpGhost :: Maze -> Piece -> Piece -> [Player] -> [Player] 
tpGhost m@(z:zs) g@(PacPlayer (Ghost (GhoState (a,(x1,y1),b,ori,c,d) Dead))) (Food Big) (x:xs)
 | (getPieceGhostID g == getPlayerID x) && even (length m) = Ghost (GhoState (a,((getMiddle m)-1,div (length z) 2),(b*2),U,c,d) Alive) : xs
 | (getPieceGhostID g == getPlayerID x) && odd (length m) = Ghost (GhoState (a,(getMiddle m,div (length z) 2),(b*2),U,c,d) Alive) : xs
 | otherwise = x : tpGhost m g (Food Big) xs
tpGhost m@(z:zs) g@(PacPlayer (Ghost (GhoState (a,(x1,y1),b,ori,c,d) Dead))) p (x:xs)
 | (getPieceGhostID g == getPlayerID x) && even (length m) = Ghost (GhoState (a,((getMiddle m)-1,div (length z) 2),(b*2),U,c,d) Alive) : xs
 | (getPieceGhostID g == getPlayerID x) && odd (length m) = Ghost (GhoState (a,(getMiddle m,div (length z) 2),(b*2),U,c,d) Alive) : xs
 | otherwise = x : tpGhost m g p xs
tpGhost m@(z:zs) _ _ (x:xs) = (x:xs)

-- | Função que dada uma peça determina a ação do jogador
determinateAction :: Piece -> Action 
determinateAction (PacPlayer (Pacman (PacState _ tm m pm))) = PacmanPlayer
determinateAction Wall = HitWall
determinateAction Empty = EmptySlot
determinateAction (Food Little) = Eat Little
determinateAction (Food Big) = Eat Big
determinateAction (PacPlayer (Ghost (GhoState _ Alive))) = LoseLife
determinateAction (PacPlayer (Ghost (GhoState _ Dead))) = EatGhost

-- | Função que dadas duas ações e um jogador, aplica essa ação no estado do jogador. Novamente, esta função recebe duas ações, devido ao facto de o pacman poder ter de realizar duas ações em simultâneo.
action :: Action -> Action -> Player -> Player
action _ _ (Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) = (Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) 
action _ _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying)) = (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying))
action EatGhost (Eat Little) (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = (Pacman (PacState (a,(x,y),b,ori,c+11,d) tm m pm))
action EatGhost (Eat Big) (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = (Pacman (PacState (a,(x,y),b,ori,c+15,d) tm m Mega))
action EatGhost _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = Pacman (PacState (a,(x,y),b,ori,c+10,d) tm m pm)
action PacmanPlayer _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)
action HitWall _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)
action EmptySlot _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)
action (Eat Little) _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) = Pacman (PacState (a,(x,y),b,ori,c+1,d) tm m pm)
action (Eat Big) _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Mega)) = Pacman (PacState (a,(x,y),b,ori,c+5,d) 10000 m Mega)
action (Eat Big) _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Normal)) = Pacman (PacState (a,(x,y),b,ori,c+5,d) 10000 m Mega)
action LoseLife _ (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) 
 | d > 0 = Pacman (PacState (a,(x,y),b,ori,c,d-1) tm m pm)
 | d == 0 = Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying)

-- | Função que aplica uma ação a uma lista de jogador, por ex : quando uma comida grande é comida, os fantasma ficam mortos. Novamente, utilizamos a estratégia enunciada na função _action_ e na função _tpGhost_
actionList :: Action -> Action -> [Player] -> [Player] 
actionList _ _ [] = []
actionList EatGhost (Eat Big) (Ghost (GhoState (a,(x,y),b,ori,c,d) gm) : xs) = Ghost (GhoState (a,(x,y),0.5,ori,c,d) Dead) : actionList EatGhost (Eat Big) xs 
actionList EatGhost (Eat Big) (x:xs) = x : actionList EatGhost (Eat Big) xs
actionList (Eat Big) ac (Ghost (GhoState (a,(x,y),b,ori,c,d) gm) : xs) = Ghost (GhoState (a,(x,y),0.5,ori,c,d) Dead) : actionList (Eat Big) ac xs 
actionList (Eat Big) ac (x:xs) = x : actionList (Eat Big) ac xs
actionList _ _ (x:xs) = (x:xs)

-- | Função que contempla todos os casos de movimentação do jogo em função da próxima peça e de uma jogada
coordsAfterPlay :: Maze -> Play -> Player -> Piece -> (Player,Piece) 
coordsAfterPlay mz@(z:zs) (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) piece 
 | o == ori && o == L && odd (length mz) && compareCoords (x,y) ((div (length mz) 2),0) = (Pacman (PacState (a,((div (length mz) 2),(length z)-1),b,L,c,d) tm m pm),piece)
 | o == ori && o == R && odd (length mz) && compareCoords (x,y) ((div (length mz) 2),(length z)-1) = (Pacman (PacState (a,((div (length mz) 2),0),b,R,c,d) tm m pm),piece)
 | o == ori && o == L && even (length mz) && compareCoords (x,y) ((div (length mz) 2),0) = (Pacman (PacState (a,((div (length mz) 2),(length z)-1),b,L,c,d) tm m pm),piece)
 | o == ori && o == L && even (length mz) && compareCoords (x,y) ((div (length mz) 2)-1,0) = (Pacman (PacState (a,((div (length mz) 2)-1,(length z)-1),b,L,c,d) tm m pm),piece)
 | o == ori && o == R && even (length mz) && compareCoords (x,y) (div (length mz) 2,(length z)-1) = (Pacman (PacState (a,(div (length mz) 2,0),b,R,c,d) tm m pm),piece)
 | o == ori && o == R && even (length mz) && compareCoords (x,y) ((div (length mz) 2)-1,(length z)-1) = (Pacman (PacState (a,((div (length mz) 2)-1,0),b,R,c,d) tm m pm),piece)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying)) piece 
 | o /= getPlayerOrientation p = (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying),piece)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying)) piece 
 | o == ori = (Pacman (PacState (a,(x,y),b,ori,c,d) tm m Dying),piece) 
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) piece 
 | o /= getPlayerOrientation p = (Pacman (PacState (a,(x,y),b,o,c,d) tm m pm),piece)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) Wall 
 | o == ori = (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm),Wall) 
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) Empty
 | o == ori && o == L = (Pacman (PacState (a,(x,y-1),b,o,c,d) tm m pm),Empty)
 | o == ori && o == R = (Pacman (PacState (a,(x,y+1),b,o,c,d) tm m pm),Empty)
 | o == ori && o == U = (Pacman (PacState (a,(x-1,y),b,o,c,d) tm m pm),Empty)
 | o == ori && o == D = (Pacman (PacState (a,(x+1,y),b,o,c,d) tm m pm),Empty)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) (Food Big)
 | o == ori && o == L = (Pacman (PacState (a,(x,y-1),b,o,c,d) tm m pm),Food Big)
 | o == ori && o == R = (Pacman (PacState (a,(x,y+1),b,o,c,d) tm m pm),Food Big)
 | o == ori && o == U = (Pacman (PacState (a,(x-1,y),b,o,c,d) tm m pm),Food Big)
 | o == ori && o == D = (Pacman (PacState (a,(x+1,y),b,o,c,d) tm m pm),Food Big)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) (Food Little)
 | o == ori && o == L = (Pacman (PacState (a,(x,y-1),b,o,c,d) tm m pm),Food Little)
 | o == ori && o == R = (Pacman (PacState (a,(x,y+1),b,o,c,d) tm m pm),Food Little)
 | o == ori && o == U = (Pacman (PacState (a,(x-1,y),b,o,c,d) tm m pm),Food Little)
 | o == ori && o == D = (Pacman (PacState (a,(x+1,y),b,o,c,d) tm m pm),Food Little)
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) (PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Alive)))
 | o == ori && o == L = (Pacman (PacState (a,(x,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Alive)))
 | o == ori && o == R = (Pacman (PacState (a,(x,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Alive)))
 | o == ori && o == U = (Pacman (PacState (a,(x,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Alive)))
 | o == ori && o == D = (Pacman (PacState (a,(x,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Alive)))
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) (PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Dead)))
 | o == ori && o == L = (Pacman (PacState (a,(x,y-1),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Dead)))
 | o == ori && o == R = (Pacman (PacState (a,(x,y+1),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Dead)))
 | o == ori && o == U = (Pacman (PacState (a,(x-1,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Dead)))
 | o == ori && o == D = (Pacman (PacState (a,(x+1,y),b,o,c,d) tm m pm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,o1,c1,d1) Dead)))
coordsAfterPlay mz (Move id o) p@(Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm)) (PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == L = (Pacman (PacState (a,(x,y-1),b,o,c,d) tm m pm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == R = (Pacman (PacState (a,(x,y+1),b,o,c,d) tm m pm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == U = (Pacman (PacState (a,(x-1,y),b,o,c,d) tm m pm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == D = (Pacman (PacState (a,(x+1,y),b,o,c,d) tm m pm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) piece 
 | o /= getPlayerOrientation p = (Ghost (GhoState (a,(x,y),b,o,c,d) gm),piece)
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) Wall 
 | o == ori = (Ghost (GhoState (a,(x,y),b,o,c,d) gm),Wall)
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) Empty
 | o == ori && o == L = (Ghost (GhoState (a,(x,y-1),b,o,c,d) gm),Empty)
 | o == ori && o == R = (Ghost (GhoState (a,(x,y+1),b,o,c,d) gm),Empty)
 | o == ori && o == U = (Ghost (GhoState (a,(x-1,y),b,o,c,d) gm),Empty)
 | o == ori && o == D = (Ghost (GhoState (a,(x+1,y),b,o,c,d) gm),Empty)
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) (Food Big)
 | o == ori && o == L = (Ghost (GhoState (a,(x,y-1),b,o,c,d) gm),Food Big)
 | o == ori && o == R = (Ghost (GhoState (a,(x,y+1),b,o,c,d) gm),Food Big)
 | o == ori && o == U = (Ghost (GhoState (a,(x-1,y),b,o,c,d) gm),Food Big)
 | o == ori && o == D = (Ghost (GhoState (a,(x+1,y),b,o,c,d) gm),Food Big)
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) (Food Little)
 | o == ori && o == L = (Ghost (GhoState (a,(x,y-1),b,o,c,d) gm),Food Little)
 | o == ori && o == R = (Ghost (GhoState (a,(x,y+1),b,o,c,d) gm),Food Little)
 | o == ori && o == U = (Ghost (GhoState (a,(x-1,y),b,o,c,d) gm),Food Little)
 | o == ori && o == D = (Ghost (GhoState (a,(x+1,y),b,o,c,d) gm),Food Little)
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) (PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,ori1,c1,d1) gm1)))
 | o == ori && o == L = (Ghost (GhoState (a,(x,y-1),b,o,c,d) gm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,ori1,c1,d1) gm1)))
 | o == ori && o == R = (Ghost (GhoState (a,(x,y+1),b,o,c,d) gm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,ori1,c1,d1) gm1)))
 | o == ori && o == U = (Ghost (GhoState (a,(x-1,y),b,o,c,d) gm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,ori1,c1,d1) gm1)))
 | o == ori && o == D = (Ghost (GhoState (a,(x+1,y),b,o,c,d) gm),PacPlayer (Ghost (GhoState (a1,(x1,y1),b1,ori1,c1,d1) gm1)))
coordsAfterPlay mz (Move id o) p@(Ghost (GhoState (a,(x,y),b,ori,c,d) gm)) (PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == L = (Ghost (GhoState (a,(x,y-1),b,o,c,d) gm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == R = (Ghost (GhoState (a,(x,y+1),b,o,c,d) gm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == U = (Ghost (GhoState (a,(x-1,y),b,o,c,d) gm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))
 | o == ori && o == D = (Ghost (GhoState (a,(x+1,y),b,o,c,d) gm),PacPlayer (Pacman (PacState (a1,(x1,y1),b1,o1,c1,d1) tm1 m1 pm1)))

















