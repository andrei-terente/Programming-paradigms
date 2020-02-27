{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S
import Data.Maybe

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Nil {parent :: Maybe (Node s a)} | Root
                | Node{ state :: s
                    , action :: Maybe a
                    , parent :: Maybe (Node s a)
                    , depth :: Int
                    , children :: [Node s a]
                    }

instance (Eq s) => Eq (Node s a) where
    (==) n1 n2 = (nodeState n1) == (nodeState n2)

instance (Show s) => Show (Node s a) where
    show node = "NODE: \n" ++ show (state node) ++ (show (children node))
{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState node = state node

emptyNode :: (ProblemState s a) => s -> Node s a
emptyNode s = (Node s Nothing Nothing 0 [])

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

getSuccesorsListAsNodes :: (ProblemState s a, Eq s) => Node s a -> [Node s a]
getSuccesorsListAsNodes node@(Node s a Nothing d c) = map (\x -> (Node (snd x) (Just (fst x)) (Just node) ((depth node) + 1) [])) succs
                        where succs = (successors (nodeState node))
getSuccesorsListAsNodes node@(Node _ _ p _ _)
                        | (isGoal (nodeState node)) == True = []
                        | (null succs) == True = []
                        | otherwise = (map (\x -> (Node (snd x) (Just (fst x)) (Just node) ((depth node) + 1)) []) succs)
                        where succs = filter (\x -> (snd x) /= (nodeState (fromJust (parent node)))) (successors (nodeState node))

createHierarchy :: (ProblemState s a, Eq s) => Node s a -> Node s a
createHierarchy (Node nState nAction nParent nDepth nChildren) = (Node nState nAction nParent nDepth (succss))
                        where succss = getSuccesorsListAsNodes (Node nState nAction nParent nDepth nChildren)

doBFS :: (ProblemState s a, Eq s, Ord s) => S.Set s -> Node s a -> [Node s a] -> Node s a
doBFS visited crt_node [] = crt_node
doBFS visited crt_node (x:xs) | isGoal (nodeState x) = doBFS visited x []
                              | (S.member (nodeState x) visited) = doBFS visited crt_node xs
                              | otherwise = doBFS (S.insert (nodeState x) visited) linked_x (xs ++ (children linked_x))
                            where linked_x = createHierarchy x

recGen :: (ProblemState s a, Eq s, Ord s) => S.Set s -> [Node s a] -> [Node s a]
recGen visited myNodes | (foldl (||) (False) (map isGoalNode myNodes)) || (null (filter (\z -> (depth z) < 10) myNodes)) = (filter isGoalNode myNodes) 
                       | otherwise = filter (\y -> (S.notMember (nodeState y) visited)) (map (\x -> (Node (nodeState x) (action x) (parent x) (depth x)) (recGen (S.insert (nodeState x) visited) (children x))) linked_nodes)
                       where linked_nodes = map createHierarchy myNodes

isGoalNode :: (ProblemState s a) => Node s a -> Bool
isGoalNode Root = False
isGoalNode (Nil s) = False
isGoalNode (Node s a p d c) = isGoal s

createStateSpace :: (ProblemState s a, Eq s, Ord s) => s -> Node s a
createStateSpace state = createHierarchy (emptyNode state)
-- createStateSpace state = (Node (nodeState root) (action root) (parent root) (depth root) (recGen (S.singleton state) (children root)))
--                          where root = createHierarchy (emptyNode state)

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri

limitedDfs node max_depth = checkCorrect (limitedDfs1 node [] max_depth)

limitedDfs1 :: (ProblemState s a, Eq s) => Node s a -> [s] -> Int -> [Node s a]
limitedDfs1 node visited 0 
                            | (elem (nodeState node) visited) = []
                            | otherwise = [node]
limitedDfs1 node visited max_depth
                            | (elem (nodeState link) visited) = []
                            | otherwise = link : (concat (map (\x -> limitedDfs1 x updated_visited (max_depth - 1)) (filter (\x -> (not (elem (nodeState x) updated_visited))) (children link))))
                            where link =  createHierarchy node
                                  updated_visited = (nodeState link) : visited      
                            --updated_visited = foldl (foldInsert) visited (map nodeState (children link))

applyToList :: (ProblemState s a, Eq s) => [Node s a] -> [s] -> Int -> [Node s a]
applyToList [] _ _ = []
applyToList nodes visited 0 = nodes
applyToList (x:xs) visited max_depth = (limitedDfs1 x visited 0) ++ applyToList xs ((state x):visited) max_depth 
                            
createFiniteHierarchy :: (ProblemState s a, Eq s, Ord s) => Node s a -> S.Set s -> Node s a
createFiniteHierarchy (Node nState nAction nParent nDepth nChildren) visited = (Node nState nAction nParent nDepth (new_state_succs))
                        where succss = getSuccesorsListAsNodes (Node nState nAction nParent nDepth nChildren)
                              new_state_succs = filter (\x -> (S.notMember (nodeState x) visited)) succss

foldInsert :: Ord a => S.Set a -> a -> S.Set a
foldInsert set elem = S.insert elem set
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

checkCorrect :: (Eq d) => [d] -> [d]
checkCorrect list = auxCheck list []

auxCheck :: (Eq d) => [d] -> [d] -> [d]
auxCheck [] _ = []
auxCheck (x:xs) list2
    | (x `elem` list2) = auxCheck xs list2
    | otherwise = x : auxCheck xs (x:list2)

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening node = (doBFS (S.empty) linked_node [linked_node], 20)
                          where linked_node = createHierarchy node
-- iterativeDeepening root = (first_sol, (length result) - 1)
--                         where result = iterative root 0
--                               first_sol = head (filter isGoalNode result)

iterative :: (ProblemState s a, Ord s) => Node s a -> Int -> [Node s a]
iterative node depth | has_final_state = result_at_depth
                     | otherwise = iterative node (depth + 1)
                     where result_at_depth = limitedDfs1 node [] depth 
                           has_final_state = (not (null (filter isGoalNode result_at_depth)))                            
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
myExtractPath :: Node s a -> [(a, s)]
myExtractPath crt_node | (isNothing prt) = []
                       | otherwise = ((fromJust (action crt_node)), (nodeState crt_node)) : (myExtractPath (fromJust prt))
                       where prt = (parent crt_node)

extractPath :: Node s a -> [(a, s)]
extractPath node = reverse (myExtractPath node)

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve initial_state _ = extractPath $ fst $ (iterativeDeepening (emptyNode initial_state))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))