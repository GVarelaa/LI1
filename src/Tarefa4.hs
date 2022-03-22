{- |

 = Introdução 

A tarefa 4 foi a tarefa em que começamos a entrar nos detalhes do jogo. Neste caso, foi a tarefa
onde criamos circunstâncias e condições no que toca aos efeitos da passagem do tempo nos Players. 
Generalizando, esta tarefa é responsável pela movimentação dos “Players”. 

 = Objetivos 

O nosso principal objetivo era definir uma função que recebesse como argumento um acumulador e, quando a lista fosse vazia, 
devolvesse o mesmo, que neste caso, seria um State já com as alterações feitas. Para completar esta tarefa, inserimos algumas 
funções na tarefa 2, que basicamente definem a animação da “mouth” do Pacman e completamos a função "play" para o movimento
dos fantasmas.

= Discussão e conclusão 

Esta tarefa, inicialmente, era um pouco confusa, dado que tínhamos de ter em conta o tempo para a movimentação do jogador.
Porém, depois de conseguirmos encontrar um ponto de partida, o raciocínio foi fluindo até a acabar a mesma. Acabou por ser 
uma análise mais detalhada ao jogo, para assim o próprio funcionar com sucesso.

-}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import Tarefa6

defaultDelayTime = 250 -- 250 ms

-- | Função principal responsável pela passagem do tempo 
passTime :: Int  -> State -> State
passTime step s@(State m (x:xs) lvl) = passTimeAuxBot step (x:xs) s 

-- | Função auxiliar para a passTime. 
passTimeAux :: Int -> [Player] -> State -> State
passTimeAux step [] s = s
passTimeAux step (x:xs) s 
 | (getPlayerVelocity x == 0.5 && odd (step)) && isPacman x                      = passTimeAux step xs s
 | (getPlayerVelocity x == 0.5 && (step > 0 && even (step))) && isPacman x       = passTimeAux step xs (inferePlay x s)
 | (getPlayerVelocity x == 1) && isPacman x                                      = passTimeAux step xs (inferePlay x s)
 | (getPlayerVelocity x == 0.5 && odd (step)) && not (isPacman x)                = passTimeAux step xs s
 | (getPlayerVelocity x == 0.5 && (step > 0 && even (step))) && not (isPacman x) = passTimeAux step xs (play (playByGhost (getPlayerID x) (ghostPlay s)) s)
 | (getPlayerVelocity x == 1) && not (isPacman x)                                = passTimeAux step xs (play (playByGhost (getPlayerID x) (ghostPlay s)) s)
 | otherwise = passTimeAux step xs s

-- | Função auxiliar para o bot.
passTimeAuxBot :: Int -> [Player] -> State -> State
passTimeAuxBot step [] s = s
passTimeAuxBot step (x:xs) s 
 | (getPlayerVelocity x == 0.5 && odd (step)) && isPacman x                      = passTimeAuxBot step xs s
 | (getPlayerVelocity x == 0.5 && (step > 0 && even (step))) && isPacman x       = passTimeAuxBot step xs (play (maybeToPlay (bot (getPlayerID x) s)) s)
 | (getPlayerVelocity x == 1) && isPacman x                                      = passTimeAuxBot step xs (play (maybeToPlay (bot (getPlayerID x) s)) s)
 | (getPlayerVelocity x == 0.5 && odd (step)) && not (isPacman x)                = passTimeAuxBot step xs s
 | (getPlayerVelocity x == 0.5 && (step > 0 && even (step))) && not (isPacman x) = passTimeAuxBot step xs (play (playByGhost (getPlayerID x) (ghostPlay s)) s)
 | (getPlayerVelocity x == 1) && not (isPacman x)                                = passTimeAuxBot step xs (play (playByGhost (getPlayerID x) (ghostPlay s)) s)
 | otherwise = passTimeAux step xs s

-- | Função que mantém a direção do jogador 
inferePlay :: Player -> State -> State
inferePlay p s@(State m ps lvl)
 | getPlayerOrientation p == R = play (Move (getPlayerID p) R) s
 | getPlayerOrientation p == L = play (Move (getPlayerID p) L) s
 | getPlayerOrientation p == U = play (Move (getPlayerID p) U) s
 | getPlayerOrientation p == D = play (Move (getPlayerID p) D) s

-- | Função que retorna a Play de um Maybe Play  
maybeToPlay :: Maybe Play -> Play
maybeToPlay (Just p) = p

-- | Função que determina a play corresponde ao id
playByGhost :: Int -> [Play] -> Play
playByGhost id1 (Move id2 ori :xs)
 | id1 == id2 = Move id2 ori
 | otherwise = playByGhost id1 xs
