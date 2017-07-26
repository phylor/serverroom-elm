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
