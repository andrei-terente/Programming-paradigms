{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import Data.Maybe
import Data.List

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

{-
    Functie ce transforma un Char intr-un String prin
    concatenarea sa la o lista vida.
-}
charToString :: Char -> String
charToString = (:[])

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HARD_TILE | SOFT_TILE | BLOCK | SWITCH | EMPTY_SPACE | WINNING_TILE
    deriving (Eq, Ord)

instance Show Cell where
    show HARD_TILE = charToString hardTile
    show SOFT_TILE = charToString softTile
    show BLOCK = charToString block
    show SWITCH = charToString switch
    show EMPTY_SPACE = charToString emptySpace
    show WINNING_TILE = charToString winningTile

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

{-
    Switch data type, where:
        - position = the position of the switch on the game board
        - tiles = positions commanded by the switch
        - off = activation state of the switch
-}
data Switch = Switch { position :: Position
                     , tiles :: [Position]
                     , off :: Bool
                     } deriving (Eq, Ord, Show)

{-
    Level data type, where:
        - board = an array that represents a matrix that contains
                  the specific Cell at each position
        - size = the board's dimensions (== A.bounds board)
        - b1 = position of the first segment of the block
        - b2 = position of the second segment of the bock
               (when the block is standing vertically, b1 == b2)
        - swtichesList = list of existing switches, stored as Switch types
        - gameWon, gameEnded = (False, False) -> the game hasn't been won/lost in this state
                             = (False, True) -> the game has been lost in this state
                             = (True, True) -> the game has been won in this state
-}
data Level = Level { board :: (A.Array Position Cell)
                   , size :: Position
                   , b1 :: Position
                   , b2 :: Position
                   , switchesList :: [Switch]
                   , gameWon :: Bool
                   , gameEnded :: Bool
                   } deriving (Ord)

instance Eq Level where
    (==) l1 l2 = segments_eq && boards_eq
                where segments_eq = ((b1 l1) == (b1 l2) && (b2 l1) == (b2 l2)) || ((b1 l1 == b2 l2) && (b1 l2 == b2 l1))
                      boards_eq = (board l1) == (board l2)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefinedl

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO *** - Done

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

{-
    Determining in which state the game is in (won, lost or ongoing), appending state specific string, then printing the
    Array (with the block beng positioned on the board) in matrix format.
-}
instance Show Level where
    show (Level arr s pos1 pos2 _ True True) = "\n" ++ (unlines [concat [show ((setValue pos1 BLOCK (setValue pos2 BLOCK arr)) 
                                                    A.! (y, x)) | x <- [0..snd s]] | y <- [0..fst s]]) ++ "Congrats! You won!\n"

    show (Level arr s pos1 pos2 _ False True) = "\n" ++ (unlines [concat [show ((setValue pos1 BLOCK (setValue pos2 BLOCK arr)) 
                                                    A.! (y, x)) | x <- [0..snd s]] | y <- [0..fst s]]) ++ "Game Over\n"

    show (Level arr s pos1 pos2 _ _ False) = "\n" ++ (unlines [concat [show ((setValue pos1 BLOCK (setValue pos2 BLOCK arr)) 
                                                    A.! (y, x)) | x <- [0..snd s]] | y <- [0..fst s]])

{-
    Auxiliary functions which change one or multiple values in an Array.
-}
setValues :: [(Int, Int)] -> Cell -> (A.Array Position Cell) -> (A.Array Position Cell)
setValues modPositions e arr = foldl (setTile e) arr modPositions

setValue :: (Int, Int) -> Cell -> (A.Array Position Cell) -> (A.Array Position Cell)
setValue (x,y) a ar = ar A.// [((x,y), a)]

{-
    Another auxiliary function that was later used to implement setValues in a
    non-recursive manner. Its funcionality is the same as setValue, its args
    being reordered to facilitate its use with fold.
-}
setTile :: Cell -> (A.Array Position Cell) -> Position -> (A.Array Position Cell)
setTile a ar (x, y) = ar A.// [((x,y), a)]

{-
    *** TODO *** - Done

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}
emptyLevel :: Position -> Position -> Level
emptyLevel boundary blockPos = (Level (setValue blockPos EMPTY_SPACE 
                                        (A.array ((0, 0), boundary) 
                                            ([((i, j), EMPTY_SPACE) 
                                            | i <- [0..fst boundary], j <- [0..snd boundary]])))
                                      boundary
                                      blockPos 
                                      blockPos 
                                      [] 
                                      False 
                                      False)

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile c pos (Level arr size1 pos1 pos2 switches won ended)
                | c == 'H' = (Level (setValue pos HARD_TILE arr) size1 pos1 pos2 switches won ended)
                | c == 'S' = (Level (setValue pos SOFT_TILE arr) size1 pos1 pos2 switches won ended)
                | c == 'W' = (Level (setValue pos WINNING_TILE arr) size1 pos1 pos2 switches won ended)
                | otherwise = (Level arr size1 pos1 pos2 switches won ended)


{-
    Removes the switch at position 'switch_pos' from the list of switches and
    returns the resulting list.
-}
removeSwitch :: Position -> [Switch] -> [Switch]
removeSwitch switch_pos switches = filter (\x -> (position x) /= switch_pos) switches

{-
    Sets the activated state of the wanted switch in the switches list to the opposite of
    its current state and returns the resulting list. 
-}
flickSwitch :: Switch -> [Switch] -> [Switch]
flickSwitch (Switch p t o) list = (Switch p t (not o)) : (removeSwitch p list)

{-
    Searches for the switch on the position 'switch_pos' in the switches list and returns:
        - Nothing: if there is no such switch in the list
        - Just Swtich: otherwise
-}
getSwitch :: Position -> [Switch] -> Maybe Switch
getSwitch switch_pos switches | idx == Nothing = Nothing
                              | otherwise = Just (switches !! (fromJust idx))
                            where idx = (findIndex (== switch_pos) (map position switches))

{-
    Based on the current state of the switch, it either replaces its commanded tiles
    with HARD_TILE cells (Off -> On) or with EMPTY_SPACE cells (On -> Off)
-}
updateTiles :: (A.Array Position Cell) -> [Position] -> Bool -> (A.Array Position Cell)
updateTiles arr positions state_off = setValues positions cell arr
                            where cell = if (state_off == True) 
                                            then HARD_TILE 
                                            else EMPTY_SPACE
{-
    Auxiliary function that creates a switch and returns it.
-}
makeSwitch :: Position -> [Position] -> Switch
makeSwitch switch1 commanded = (Switch switch1 commanded True)

{-
    *** TODO *** - Done

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switch1 commanded (Level arr size1 pos1 pos2 switches won ended)
                            = (Level updatedArray size1 pos1 pos2 updatedList won ended)
                                     where updatedArray = (setValue switch1 SWITCH arr) 
                                           updatedList = (makeSwitch switch1 commanded) : switches

{-
    === MOVEMENT ===
-}

{-
    *** TODO *** - Done

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

{-
    Tries to activate the switch at 'switch_pos'.
    If there is no such switch, the level with no modification is returned,
    else, it returns level with its board and switches list modified accordingly.
-}
activateOneBlock :: Position -> Level -> Level
activateOneBlock switch_pos (Level arr size1 pos1 pos2 switches won ended)
            | get_switch == Nothing = (Level arr size1 pos1 pos2 switches won ended)
            | otherwise = (Level (updateTiles arr (tiles $ fromJust $ get_switch) (off $ fromJust $ get_switch)) 
                                 size1 pos1 pos2 
                                 (flickSwitch (fromJust get_switch) switches) 
                                 won ended)
            where get_switch = getSwitch switch_pos switches

{-
    Determines the block's orientation (verical / horizontal) and:
        - if the block is standing vertically: tries to activate the switch on just one
                                               of its segments' positions (either one).
        - if the block is standing horizontally: tries to activate the switches on both
                                               of its segments' positions.
-}
activate :: Cell -> Level -> Level
activate _ level
            | vertical == False = activateOneBlock (b1 $ level) (activateOneBlock (b2 $ level) level)
            | otherwise = activateOneBlock (b2 $ level) level
            where vertical = ((b1 $ level) == (b2 $ level))

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

{-
    Checks if the game in the current state is won / lost and returns its status.
-}
gameStatus :: Level -> (Bool, Bool)
gameStatus (Level arr _ pos1 pos2 _ _ _)
                                        | pos1 == pos2 && (arr A.! pos1) == WINNING_TILE = (True, True)
                                        | pos1 == pos2 && (arr A.! pos1) == SOFT_TILE = (False, True)
                                        | (arr A.! pos1) == EMPTY_SPACE || (arr A.! pos2) == EMPTY_SPACE = (False, True)
                                        | otherwise = (False, False)

{-
    Updates the game's status based on a (Bool, Bool) pair given as arg.
-}
updateStatus :: Level -> (Bool, Bool) -> Level
updateStatus (Level arr size1 pos1 pos2 switches _ _) (new_won, new_ended) = (Level arr size1 pos1 pos2 switches new_won new_ended)

{-
    Moving in either direction means determining the blocks position and orientation based
    on the positions of its segments:
            - vertical orientation, b1 == b2
            - horizontal orientation, segments being on the same row of the board
            - horizontal orientation, segments being on the same column of the board
    
    After this was determined, the segments' positions are updated, then activate is called
    on the new level, and finally, after activating the switches, the game status (won/lost/ongoing)
    is updated.
-}
move :: Directions -> Level -> Level
move _ (Level arr size1 pos1 pos2 switches won True) = (Level arr size1 pos1 pos2 switches won True)

move North (Level arr size1 pos1 pos2 switches won ended)
                                        | pos1 == pos2 = updateStatus (activate SWITCH move1) (gameStatus (move1))
                                        | (fst pos1) == (fst pos2) = updateStatus (activate SWITCH move2) (gameStatus (move2))
                                        | ((snd pos1) == (snd pos2) && (fst pos1) < (fst pos2)) = updateStatus (activate SWITCH move3) (gameStatus move3)
                                        | otherwise = updateStatus (activate SWITCH move4) (gameStatus (move4))
                                        where move1 = (Level arr size1 (((fst pos1) - 1), (snd pos1)) (((fst pos2) - 2), (snd pos2)) switches won ended)
                                              move2 = (Level arr size1 (((fst pos1) - 1), (snd pos1)) (((fst pos2) - 1), (snd pos2)) switches won ended)
                                              move3 = (Level arr size1 (((fst pos1) - 1), (snd pos1)) (((fst pos2) - 2), (snd pos2)) switches won ended)
                                              move4 = (Level arr size1 (((fst pos1) - 2), (snd pos1)) (((fst pos2) - 1), (snd pos2)) switches won ended)

move South (Level arr size1 pos1 pos2 switches won ended)
                                        | pos1 == pos2 = updateStatus (activate SWITCH move1) (gameStatus (move1))
                                        | (fst pos1) == (fst pos2) = updateStatus (activate SWITCH move2) (gameStatus (move2))
                                        | ((snd pos1) == (snd pos2) && (fst pos1) < (fst pos2)) = updateStatus (activate SWITCH move3) (gameStatus move3)
                                        | otherwise = updateStatus (activate SWITCH move4) (gameStatus (move4))
                                        where move1 = (Level arr size1 (((fst pos1) + 1), (snd pos1)) (((fst pos2) + 2), (snd pos2)) switches won ended)
                                              move2 = (Level arr size1 (((fst pos1) + 1), (snd pos1)) (((fst pos2) + 1), (snd pos2)) switches won ended)
                                              move3 = (Level arr size1 (((fst pos1) + 2), (snd pos1)) (((fst pos2) + 1), (snd pos2)) switches won ended)
                                              move4 = (Level arr size1 (((fst pos1) + 1), (snd pos1)) (((fst pos2) + 2), (snd pos2)) switches won ended)

move West (Level arr size1 pos1 pos2 switches won ended)
                                        | pos1 == pos2 = updateStatus (activate SWITCH move1) (gameStatus (move1))
                                        | (snd pos1) == (snd pos2) = updateStatus (activate SWITCH move2) (gameStatus (move2))
                                        | ((fst pos1) == (fst pos2) && (snd pos1) < (snd pos2)) = updateStatus (activate SWITCH move3) (gameStatus move3)
                                        | otherwise = updateStatus (activate SWITCH move4) (gameStatus (move4))
                                        where move1 = (Level arr size1 ((fst pos1), (snd pos1) - 1) ((fst pos2), (snd pos2) - 2) switches won ended)
                                              move2 = (Level arr size1 ((fst pos1), (snd pos1) - 1) ((fst pos2), (snd pos2) - 1) switches won ended)
                                              move3 = (Level arr size1 ((fst pos1), (snd pos1) - 1) ((fst pos2), (snd pos2) - 2) switches won ended)
                                              move4 = (Level arr size1 ((fst pos1), (snd pos1) - 2) ((fst pos2), (snd pos2) - 1) switches won ended)

move East (Level arr size1 pos1 pos2 switches won ended)
                                        | pos1 == pos2 = updateStatus (activate SWITCH move1) (gameStatus move1)
                                        | (snd pos1) == (snd pos2) = updateStatus (activate SWITCH move2) (gameStatus move2)
                                        | ((fst pos1) == (fst pos2) && (snd pos1) < (snd pos2)) = updateStatus (activate SWITCH move3) (gameStatus move3)
                                        | otherwise = updateStatus (activate SWITCH move4) (gameStatus move4)
                                        where move1 = (Level arr size1 ((fst pos1), (snd pos1) + 1) ((fst pos2), (snd pos2) + 2) switches won ended)
                                              move2 = (Level arr size1 ((fst pos1), (snd pos1) + 1) ((fst pos2), (snd pos2) + 1) switches won ended)
                                              move3 = (Level arr size1 ((fst pos1), (snd pos1) + 2) ((fst pos2), (snd pos2) + 1) switches won ended)
                                              move4 = (Level arr size1 ((fst pos1), (snd pos1) + 1) ((fst pos2), (snd pos2) + 2) switches won ended)

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level _ _ _ _ _ _ False) = True
continueGame (Level _ _ _ _ _ _ True) = False

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}
gameIsntLost :: Level -> Bool
gameIsntLost (Level _ _ _ _ _ False True) = False
gameIsntLost (Level _ _ _ _ _ _ _) = True

getAllSuccesors :: Level -> [(Directions, Level)]
getAllSuccesors level = map (\dir -> (dir, move dir level)) [East, South, West, North]

removeDups :: (Eq a) => [a] -> [a]
removeDups l = reverse (rdHelper [] l)
        where rdHelper visited [] = visited
              rdHelper visited (x:xs)
                    | x `elem` visited = rdHelper visited xs
                    | otherwise = rdHelper (x : visited) xs

instance ProblemState Level Directions where
    successors (Level _ _ _ _ _ _ True) = []
    successors level@(Level _ _ _ _ _ _ False) = filter (\x -> (gameIsntLost (snd x))) (getAllSuccesors level)
                              

    isGoal level = (gameWon level)

    -- Doar petru BONUS
    -- heuristic = undefined
