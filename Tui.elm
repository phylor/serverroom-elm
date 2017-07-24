module Tui exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Menu messageType =
  { options : List (MenuOption messageType)
  , selectedOption : MenuOption messageType
  }

type alias MenuOption messageType =
  { title : String
  , action : messageType
  }

createWindow : String -> Int -> Int -> Int -> Int -> Svg message
createWindow title xpos ypos w h =
  g []
    [ rect [ x <| toString xpos, y <| toString ypos, width <| toString w, height <| toString h, Svg.Attributes.style "fill: rgb(21, 3, 183)" ] []
    , rect [ x <| toString xpos, y <| toString ypos, width <| toString w, height "20", Svg.Attributes.style "fill: rgb(0, 178, 181)" ] []
    , text_ [ x <| toString (xpos + 3), y <| toString (ypos + 15), Svg.Attributes.style "font-family:sans-serif" ] [ text title ]
    ]

createMenu : List (MenuOption messageType) -> Svg message
createMenu options =
  g []
    (List.map (\item -> text_ [] []) options)
