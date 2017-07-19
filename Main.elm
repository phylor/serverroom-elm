module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)

import Grid exposing (render)

type Msg = NoOp

type alias Position =
  ( Int, Int )

type alias Model =
  { playerPosition : Position
  }

init =
  ( Model (1, 1), Cmd.none )

update msg model =
  ( model, Cmd.none )

view model =
  svg [ width "500", height "500" ]
    [ renderBackground
    , Grid.render
    , renderPlayer model.playerPosition
    ]

styl =
  Svg.Attributes.style

renderBackground =
  rect [ width "500", height "500", styl "fill: rgb(200, 200, 200)" ] []

renderPlayer playerPosition =
  let
    playerX = (first playerPosition - 1) * 50
    playerY = (second playerPosition - 1) * 50
  in
    image [ x <| toString (playerX + 2), y <| toString (playerY + 2), width "45", height "45", xlinkHref "resources/engineer.svg" ] []

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = \model -> Sub.none
  }
