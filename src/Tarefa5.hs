{- |

= Introdução
Nesta tarefa, o objetivo principal é implementarmos diversos comportamentos para os fantasmas, 
tendo de complementar a tarefa 2 com certas funções que nos permitissem fazer as jogadas desejadas
com os fantasmas.

= Objetivos
Para fazer esta tarefa, esquematizamos aquilo que pretendíamos obter, sendo que, como era bastante 
intuitivo, não foi difícil de traduzir o nosso raciocínio para o código. Implementamos funções que 
verificam o estado do fantasma (morto ou vivo) e, por exemplo, a scatterMode e a chaseMode, que são 
duas funções importantíssimas, visto que são as responsáveis pelo que o fantasma vai fazer dada uma 
condição e a mais geral. A função "scatterMode" é responsável pelo movimento de fuga do pacman e a função
"chaseMode" é responsável pelo movimento de perseguição ao pacman.

Estratégia Scatter Mode : Utilizamos uma estratégia simples para o scatter mode, que consistem em manter
a orientação quando a próxima peça não é uma parede. Caso seja uma parede então o ghost vai acimar a orientação 
à sua direita.

Estratégia Chase Mode : Para o chase mode utilizamos uma estratégia em que quando a próxima peça não é parede 
mantém a orientação que tinha. Caso seja uma parede, então vai determinar a orientação mais próxima do pacman.

= Discussão e conclusão
Overall, esta tarefa era bastante intuitiva, o que levou a um raciocínio fluido e objetivo. O resultado 
final é o esperado, e funciona tudo na perfeição.

-}

module Tarefa5 where 

import Types
import Tarefa2
import Tarefa6

-- | Função principal que devolve a lista de jogadas dos fantasmas
ghostPlay :: State -> [Play] -- ver esta função
ghostPlay s@(State m [x] lvl) = []
ghostPlay s@(State m (x:xs) lvl)
 | isPacman x = ghostPlay (State m (xs ++ [x]) lvl)
 | getGhostMode x == Alive = (chaseMode s (getPlayerID x)) : ghostPlay (State m xs lvl) 
 | getGhostMode x == Dead  = (scatterMode s (getPlayerID x)) : ghostPlay (State m xs lvl)

-- | Função que retorna uma play que persegue o pacman
chaseMode :: State -> Int -> Play
chaseMode s@(State m pls lvl) id 
 | (isWall s id)     = Move id bestOriToChase
 | not (isWall s id) = Move id (getPlayerOrientation (selectCertainPlayer id pls))
                 where bestOriToChase = bestOriToChaseWithoutWall (oriWithoutWallsforGhost m (getPacmanCoords pls) (fourCoords (getPlayerCoords (selectCertainPlayer id pls))))

-- | Função que retorna uma play que foge do pacman
scatterMode :: State -> Int -> Play
scatterMode s@(State m pls lvl) id 
 | (isWall s id)     = Move id (guessGhostDeadOri (getPlayerOrientation (selectCertainPlayer id pls)))
 | not (isWall s id) = Move id (getPlayerOrientation (selectCertainPlayer id pls))

-- | Função que determina uma orientação clockwise
guessGhostDeadOri :: Orientation -> Orientation
guessGhostDeadOri R = D
guessGhostDeadOri D = L
guessGhostDeadOri L = U
guessGhostDeadOri U = R

















