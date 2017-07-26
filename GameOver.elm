module GameOver exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Tui exposing (..)

renderGameOver settings =
  g []
    [ renderWindow "Game Over" 0 0 settings.width settings.height
    , text_ [ x "20", y "50", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text "*** STOP: 0x000000D1 (0xE10C5678, 0x00000002, 0x00000000, 0xF8C48579)" ]
    ]

isGameOver model =
  model.game.money < 0
