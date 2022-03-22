-- | Este módulo contém todos os tipos das tarefas
module Types where

import Data.List

-- | Data que representa um estado  
data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }
-- | Tipo que representa um labirinto, ou seja, lista de corridores
type Maze = [Corridor]
-- | Tipo que representa um corridor, ou seja, lista de peças
type Corridor = [Piece]
-- | Data que indica todas as peças do jogo
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
-- | Data que indica os papeis que um jogador pode assumir no jogo (Pacman ou Ghost)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)
-- | Data que indica as ações que ocorrem no jogo
data Action = EmptySlot | Eat FoodType | HitWall | LoseLife | Death | EatGhost | PacmanPlayer   deriving (Show)

-- | Data que indica todas as orientações que um jogador pode assumir
data Orientation = L | R | U | D | Null deriving (Eq,Show)
-- | Data que representa o estado de um pacman
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

-- | Data que representa o estado de um fantasma
data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

-- | Tipo que representa as coordenadas
type Coords = (Int,Int)
-- | Tipo que representa um PlayerState
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
-- | Data que indica o estado da boca (aberta ou fechada)
data Mouth = Open | Closed deriving (Eq,Show)
-- | Data que indica o modo de um pacman
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
-- | Data que indica o modo de um ghost
data GhostMode = Dead  | Alive deriving (Eq,Show)
-- | Data que indica se uma comida é grande ou pequena
data FoodType = Big | Little deriving (Eq)
-- | Data que indica as cores
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

-- | Data que representa uma jogada
data Play = Move Int Orientation deriving (Eq,Show)

-- | Tipo que representa uma lista de Instruction
type Instructions = [Instruction]

-- | Data que representa uma instruction
data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


-- | Função que dada uma string e um cor, colore a string
coloredString :: String -> Color -> String
coloredString x y = x
{-
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 
-}

-- | Função que coloca os jogadores no labirinto
placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )

-- | Função que dado um labirinto transforma-o em string
printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

-- | Função que dado um jogador transforma em string o seu playerstate, as vidas, etc..
printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

-- | Função que dado um jogador retorna o seu id
getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x

-- | Função que dado um jogador retorna os seus pontos
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

-- | Função que dado um jogador jogador e umas coordenadas, atualiza o jogador com as coordenadas dadas
setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )
 
-- | Função que dada uma peça retorna a sua orientação
getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

-- | Função que dado um jogador pacman retorna o modo do pacman
getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState a b )) = b

-- | Função que dado um jogador retorna o seu playerstate  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerLives :: Player -> Int
getPlayerLives (Pacman (PacState (x,y,z,t,h,l) q c d )) = l 
getPlayerLives  (Ghost (GhoState (x,y,z,t,h,l) q )) = l

getPlayerVelocity :: Player -> Double
getPlayerVelocity (Pacman (PacState (x,y,z,t,h,l) q c d )) = z
getPlayerVelocity (Ghost (GhoState (x,y,z,t,h,l) q )) = z

-- | Função que dado um jogador retorna a sua orientação
getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

-- | Função que substitui uma peça nas coordenadas dadas
replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

-- | Função auxiliar para a replaceElemInMaze
replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- | Função que dado um jogador retorna as suas coordenadas
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Função que nos dá o "meio" do Labirinto
getMiddle :: Maze -> Int
getMiddle x = div (length x) 2

-- | Função que retorna o pacman da lista de players
getPacman :: [Player] -> Player
getPacman (Pacman p : xs) = Pacman p
getPacman (x:xs) = getPacman xs 

-- | Função que retorna o id do pacman da lista de players
getPacID :: [Player] -> Int
getPacID (Pacman (PacState (a,(x,y),b,ori,c,d) tm m pm) : xs) = a
getPacID (x:xs) = getPacID xs

-- | Função que verifica se um player é pacman ou não, retornando um bool
isPacman :: Player -> Bool
isPacman (Pacman _) = True
isPacman (Ghost _) = False

-- | Função que verifica se pacman está em modo mega ou não, retornando um bool
isMega :: Player -> Bool
isMega (Pacman (PacState _ _ _ Mega)) = True
isMega _ = False

-- | Função que retorna o estado da boca de um pacman
getMouth :: Player -> Mouth
getMouth (Pacman (PacState _ _ m _)) = m

-- | Função que retorna o tempo mega do pacman
getTimeMega :: [Player] -> Double
getTimeMega (Pacman (PacState _ tm _ _):xs) = tm
getTimeMega (x:xs) = getTimeMega xs

-- | Função que compara coordenadas e retorna uma booliano
compareCoords :: Coords -> Coords -> Bool 
compareCoords (x1,y1) (x2,y2) = x1==x2 && y1==y2

-- | Função que reduz o tempo mega consoante a variável passada como argumento
updateTimeMega :: Double -> Player -> Player
updateTimeMega x (Pacman (PacState ps tm m pm)) = Pacman (PacState ps (tm-x) m pm)

-- | Função que reduz o tempo mega de um pacman aplicada a uma lista
reduceTimeMega :: [Player] -> [Player]
reduceTimeMega [] = []
reduceTimeMega (x:xs)
 | isPacman x = updateTimeMega 250 x : xs
 | otherwise = x : reduceTimeMega xs

-- | Função que aplica a __reduceTimeMega__ a um state
reduceTimeMegaState :: State -> State
reduceTimeMegaState (State m pls lvl) = State m (reduceTimeMega pls) lvl

-- | Função que dá as coordenadas de um pacman através de uma lista
getPacmanCoords :: [Player] -> Coords
getPacmanCoords ((Pacman (PacState (x,y,z,t,h,l) q c d )):xs) = y
getPacmanCoords (x:xs) = getPacmanCoords xs 

-- | Função que calcula a distância perante duas coordenadas
distance :: Coords -> Coords -> Float
distance (x1,y1) (x2,y2) = sqrt ((fromIntegral x1 - fromIntegral x2)^2 + (fromIntegral y1 - fromIntegral y2)^2)

-- | Função que dá a lista dos fantasmas de um state
getGhosts :: State -> [Player]
getGhosts (State m [] lvl) = []
getGhosts (State m (Ghost (GhoState ps gm) : xs ) lvl) = Ghost (GhoState ps gm) : getGhosts (State m xs lvl)
getGhosts (State m (x:xs) lvl) = getGhosts (State m xs lvl) 

-- | Função que dá a lista dos fantasmas vivos de um state
getGhostsAlive :: State -> [Player]
getGhostsAlive (State m [] lvl) = []
getGhostsAlive (State m (Ghost (GhoState ps Alive) : xs ) lvl) = Ghost (GhoState ps Alive) : getGhosts (State m xs lvl)
getGhostsAlive (State m (x:xs) lvl) = getGhosts (State m xs lvl) 

-- | Função que dá a lista de coordenadas dos fantasmas
ghostCoords :: [Player] -> [Coords]
ghostCoords [] = []
ghostCoords (x:xs) = getPlayerCoords x : ghostCoords xs

-- | Função que verifica se há ghost Alive 
checkGhostsAlive :: [Player] -> Bool
checkGhostsAlive [] = False
checkGhostsAlive (Ghost (GhoState ps Alive) : xs) = True
checkGhostsAlive (x:xs) = checkGhostsAlive xs







