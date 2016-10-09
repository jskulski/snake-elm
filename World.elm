module World exposing (..)

{-
    The World is where Things interact.

    This is a 2D world. (x, y) with (0, 0) in the center.
-}


type alias Height = Int
type alias Width = Int

type alias X = Int
type alias Y = Int
type alias Cell = (Float, Float)


type Direction
 = North
 | East
 | South
 | West

