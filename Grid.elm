module Grid exposing (render)

import Svg exposing (g, line)
import Svg.Attributes exposing (..)

render =
  g [ stroke "rgb(230, 230, 230)" ]
    (List.concat [renderVerticalLines, renderHorizontalLines])
    
renderVerticalLines =
  xPositions |> List.map (\x -> line [ x1 x, y1 "0", x2 x, y2 "400" ] [])

renderHorizontalLines =
  yPositions |> List.map (\y -> line [ x1 "0", y1 y, x2 "600", y2 y ] [])

xPositions : List String
xPositions =
  List.range 1 11 |> List.map (\element -> toString <| 50 * element)

yPositions : List String
yPositions =
  List.range 1 7 |> List.map (\element -> toString <| 50 * element)
