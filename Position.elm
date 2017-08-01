module Position exposing (..)

import Tuple exposing (first, second)

type alias Position =
  ( Int, Int )

toPixelX : Position -> Int
toPixelX position =
  (first position - 1) * 50

toPixelY : Position -> Int
toPixelY position =
  (second position - 1) * 50 + 20

toPixelXString : Position -> String
toPixelXString position =
  toString <| toPixelX position

toPixelYString : Position -> String
toPixelYString position =
  toString <| toPixelY position

topOf (x, y) =
  if y <= 1 then
    Nothing
  else
    Just <| (x, y - 1)

bottomOf (x, y) =
  if y >= 10 then
    Nothing
  else
    Just <| (x, y + 1)

leftOf (x, y) =
  if x <= 1 then
    Nothing
  else
    Just <| (x - 1, y)

rightOf (x, y) =
  if x >= 10 then
    Nothing
  else
    Just <| (x + 1, y)
