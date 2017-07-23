module Position exposing (..)

import Tuple exposing (first, second)

type alias Position =
  ( Int, Int )

toPixelX : Position -> String
toPixelX position =
  toString <| (first position - 1) * 50

toPixelY : Position -> String
toPixelY position =
  toString <| (second position - 1) * 50


