module Dialog exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Tui exposing (..)

type alias Dialog messageType =
  { message : String
  , menu : Menu messageType
  }

renderDialog model =
  case model.dialog of
    Just dialog ->
      renderDialogAt dialog
    Nothing ->
      g [] []

renderDialogAt dialog =
  g []
    [ rect [ x "50", y "100", width "400", height "300", Svg.Attributes.style "fill: rgb(21, 3, 183)" ] []
    , text_ [ x "75", y "125", Svg.Attributes.style "fill: rgb(144, 144, 181); font-family: sans-serif" ] [ text dialog.message ]
    , renderMenu dialog.menu 150 200
    ]
