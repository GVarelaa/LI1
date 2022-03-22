module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa4
import Tarefa5
import Tarefa6

data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    }

-- | Manager predefinido   

loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime ) 

-- | Função responsável por associar uma tecla a um movimento

updateControlledPlayer :: Key -> Manager -> Manager -- TODO
updateControlledPlayer KeyUpArrow m = rotatePlayerWithID U m 
updateControlledPlayer KeyDownArrow m = rotatePlayerWithID D m
updateControlledPlayer KeyLeftArrow m = rotatePlayerWithID L m
updateControlledPlayer KeyRightArrow m = rotatePlayerWithID R m

-- | Função que, dada uma orientação, vai rodar o jogador na direção respetiva.

rotatePlayerWithID :: Orientation -> Manager -> Manager
rotatePlayerWithID o (Manager (State m ps lvl) pid s b d del) = Manager (State m (updateOrientationList (getPlayerID (getPacman ps) ) o ps) lvl) pid s b d del


updateOrientationList :: Int -> Orientation -> [Player] -> [Player]
updateOrientationList id o [] = []
updateOrientationList id o (x:xs)
 | id == getPlayerID x = (updateOrientation o x) : xs
 | otherwise = x : updateOrientationList id o xs


updateOrientation :: Orientation -> Player -> Player
updateOrientation o (Pacman (PacState (id,(x,y),v,x1,p,l) tm m pm)) = (Pacman (PacState (id,(x,y),v,o,p,l) tm m pm))
updateOrientation o (Ghost (GhoState (id,(x,y),v,x1,p,l) gm)) = (Ghost (GhoState (id,(x,y),v,o,p,l) gm))


updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render

-- | Função que nos dá o tempo atual 
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- | Função responsável pela atualização do tempo ao longo do jogo

updateTime :: Integer -> Manager -> Manager -- TODO 
updateTime now man@(Manager state pid step bef del delay) = Manager state pid step bef (now - bef) delay    -- TODO 

-- | Função responsável por reiniciar o temporizador

resetTimer :: Integer -> Manager -> Manager
resetTimer now man@(Manager state pid step bef del delay) = (Manager state pid step now 0 delay)   -- TODO  


nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step bef del delay) = Manager (reduceTimeMegaState (passTime step state)) pid (step+1) now 0 delay -- TODO 

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorWhite ColorDefault  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

-- | Função principal, responsável por dar load ao jogo

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

