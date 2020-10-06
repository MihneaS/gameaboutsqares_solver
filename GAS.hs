{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import Data.Char

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}

data Object = Square Color Heading| Circle Color | Arrow Heading | NoBack | NoFront
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}

showColor :: Color -> String
showColor Red = "r"
showColor Blue = "b"
showColor Gray = "g"

data Size = Small | Large

showColorWithSize :: Color -> Size -> String
showColorWithSize c Large = map toUpper (showColor c)
showColorWithSize c Small = map toLower (showColor c)

instance Show Object where
    show (Arrow h) = show h
    show (Circle c) = showColorWithSize c Small
    show (Square c h) = showColorWithSize c Large ++ show h
    show NoBack = " "
    show NoFront = "  "

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Cell = Cell
    {   f::Object
    ,   b::Object
    } deriving (Eq, Ord)

instance Show Cell where
    show cell = (show (f cell)) ++ (show (b cell))

data Level = Level (M.Map Position Cell)
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}
x :: Position -> Int
x (a, _) = a

y :: Position -> Int
y (_, a) = a


positionToIndex :: Position -> Position -> Int -> Int
positionToIndex (xp, yp) (xmin, ymin) width  = 4*(width*xn + yn) where
    yn = yp - ymin
    xn = xp - xmin

instance Show Level where
    show (Level lmap) = let takeMinMax (pmin, pmax) k e = ((((min (x pmin) (x k)), (min (y pmin) (y k))), ((max (x pmax) (x k)), (max (y pmax) (y k)))), e)
                            accum = ((maxBound::Int, maxBound::Int), (minBound::Int, minBound::Int))
                            in let (pmin, pmax) = fst $ M.mapAccumWithKey takeMinMax accum lmap
                                   in let width = (y pmax) - (y pmin) + 1
                                          height = (x pmax) - (x pmin) + 1
                                          in let toBePrinted = init $ take (4*width * height) $ cycle $ ((init (take (4*width) (cycle "   |"))) ++ "\n")
                                                 completeLevel a k e = let i = positionToIndex k pmin width
                                                                           in let ys = fst $ splitAt i a
                                                                                  zs = snd $ splitAt (i + 3) a
                                                                                  in (ys ++ (show e) ++ zs, e)
                                                 in fst $ M.mapAccumWithKey completeLevel toBePrinted lmap

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Level M.empty

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}

modifyHeading :: Object -> Heading -> Object
modifyHeading (Square color _) newHeading = Square color newHeading
modifyHeading _ _ = error "modify heading got argument that is not a square\n"

combine :: Cell -> Cell -> Cell
combine (Cell NoFront NoBack) old = old
combine (Cell NoFront back ) old = Cell (f old) back
combine (Cell front NoBack) (Cell _ (Arrow heading)) = Cell (modifyHeading front heading) (Arrow heading)
combine (Cell front NoBack) (Cell _ back) = Cell front back
combine new _ = new

addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading pos (Level lMap)= Level $ M.insertWith combine pos (Cell (Square color heading) NoBack) lMap

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color pos (Level lMap) = Level $ M.insertWith combine pos (Cell NoFront (Circle color)) lMap

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading pos (Level lMap) = Level $ M.insertWith combine pos (Cell NoFront (Arrow heading)) lMap

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

deleteFront :: Position -> Level -> Level
deleteFront pos (Level lMap) = let lu = M.lookup pos lMap
                                   in case lu of
                                        Nothing -> Level lMap
                                        Just (Cell _ NoBack) -> Level $ M.delete pos lMap
                                        Just (Cell _ back) -> let foo _ = Just (Cell NoFront back) in Level $ M.alter foo pos lMap


translate :: Position -> Heading -> Position
translate (py, px) North = ((py-1), px)
translate (py, px) South = ((py+1), px)
translate (py, px) East = (py, (px+1))
translate (py, px) West = (py, (px-1))

push :: Position -> Heading -> Level -> Level
push pos direction (Level lMap) = let lu = M.lookup pos lMap
                                      newpos = translate pos direction
                                      in case lu of
                                        Nothing -> Level lMap
                                        Just (Cell NoFront _) -> Level lMap
                                        Just (Cell (Square color heading) _) -> addSquare color heading newpos $ deleteFront pos $ push newpos direction $ Level lMap
                                        Just (Cell _ _) -> error "found a cell with a nonFront (Circle, Arrow or NoBack) as Front\n"

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move pos (Level lMap) = let lu = M.lookup pos lMap
                            in case lu of 
                                Nothing -> (Level lMap)
                                Just (Cell NoFront _) -> (Level lMap)
                                Just (Cell (Square _ heading) _) -> push pos heading (Level lMap)
                                Just (Cell _ _) -> error "found a cell with a nonFront (Circle, Arrow or NoBack) as Front\n"

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
multumit :: (Object, Object) -> Bool
multumit (Square scolor _, Circle ccolor) = scolor == ccolor
multumit _ = False

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

instance ProblemState Level Position where
    successors (Level lMap) = [(action, move action (Level lMap)) | (action, Cell (Square _ _) _) <- M.toList lMap]

    isGoal (Level lMap) = foldl1 (&&) $ map (\pairy -> multumit pairy) [(front, Circle ccolor) | (_, Cell front (Circle ccolor)) <- M.toList lMap]

    -- Doar petru BONUS
    heuristic (Level lMap) = foldl1 (+) [distance poss posc | (poss, Cell (Square scolor _) _) <- M.toList lMap, (posc, Cell _ (Circle ccolor)) <- M.toList lMap, scolor == ccolor]
