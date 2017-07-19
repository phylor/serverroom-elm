module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Grid exposing (render)

type Msg = NoOp

type alias Model =
  {
  }

init =
  ( Model, Cmd.none )

update msg model =
  ( model, Cmd.none )

view model =
  svg [ width "640", height "480" ]
    [ renderBackground
    , Grid.render
    ]

styl =
  Svg.Attributes.style

renderBackground =
  rect [ width "600", height "400", styl "fill: rgb(200, 200, 200)" ] []

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = \model -> Sub.none
  }
