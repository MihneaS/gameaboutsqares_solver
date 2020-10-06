{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Search where

import ProblemState

import qualified Data.Set as S

import Data.Bool as B

import Data.List as L

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = Node
    {   state::s
    ,   action::a
    ,   parent::Node s a
    ,   adancime::Int
    }
    deriving Show

instance (Eq s) => Eq (Node s a) where
    Node s1 _ _ _ == Node s2 _ _ _ = s1 == s2

instance (Ord s) => Ord (Node s a) where
    Node s1 _ _ _ <= Node s2 _ _ _ = s1 <= s2
{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState node = state node

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}
successorsAsNodes :: (ProblemState s a) => Node s a -> [Node s a]
successorsAsNodes node = map (\(act, st) -> Node st act node ((adancime node) + 1)) $ successors $ nodeState node

myLimitedDfs :: (ProblemState s a, Ord (Node s a))
           => Node s a    -- Nodul initial
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> S.Set (Node s a)  -- Setul final de noduri traversate
myLimitedDfs initial _ 0 = S.singleton initial
myLimitedDfs initial B.False maxHeight = foldl S.union (S.singleton initial) $ map (\n -> myLimitedDfs n B.False (maxHeight - 1)) $ successorsAsNodes initial
myLimitedDfs _ B.True _  = undefined
myLimitedDfs2 :: (ProblemState s a, Ord (Node s a))
           => Node s a    -- Nodul initial
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Setul final de noduri traversate
myLimitedDfs2 initial B.False 0 = [initial]
myLimitedDfs2 initial B.False maxHeight = [initial] ++  (concatMap (\n -> myLimitedDfs2 n B.False (maxHeight - 1)) (successorsAsNodes initial))
myLimitedDfs2 initial B.True maxHeight = sortBy (\n1 n2 -> compare (heuristic (nodeState n1)) (heuristic (nodeState n2))) $ [initial] ++  (concatMap (\n -> myLimitedDfs2 n B.False (maxHeight - 1)) (successorsAsNodes initial))

limitedDfs :: (ProblemState s a, Ord s, Ord (Node s a))
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs initial boolean maxHeight = L.nub $ myLimitedDfs2 (Node initial undefined undefined 0 ) boolean maxHeight


{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}

iterativeDeepening :: (ProblemState s a, Ord s, Ord a, Ord (Node s a))
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening initial boolean  = let results = concatMap (limitedDfs initial boolean ) [1..]
                                        in let (covered, goals) = break (isGoal . nodeState) results
                                             in (head goals, length covered + 1)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
nodeToAS :: Node s a -> (a, s)
nodeToAS node = (action node, nodeState node)

extractPath :: Node s a -> [(a, s)]
extractPath node = map nodeToAS $ foldr (\_ acum-> [(parent (head acum))] ++ acum) [node] [1..((adancime node)-1)]

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
