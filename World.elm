module World exposing (..)

import Array

{-
    The World is where Things interact.
    It is a 2D world, (x, y) with (0, 0) in the center.
-}


type alias Height = Int
type alias Width = Int

type alias X = Int
type alias Y = Int
type alias Cell = (Float, Float)


type alias World =
    Array.Array (Array.Array Cell)


type Direction
 = North
 | East
 | South
 | West

