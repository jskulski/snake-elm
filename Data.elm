module Data exposing (..)

{-
    Leaf nodes on the object graph.
    Pure data objects only, please!
-}


type alias Height = Int
type alias Width = Int


type alias Position = (Float, Float)


type Direction
 = North
 | East
 | South
 | West

