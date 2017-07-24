module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Keyboard exposing (..)
import Char exposing (fromCode)

import Messages exposing (..)
import Infrastructure exposing (..)
import Position exposing (..)
import Dialog exposing (..)
import Game exposing (..)
import Grid exposing (render)
import Player exposing (moveLeft, moveUp, moveRight, moveDown)

type alias Model =
  { game : GameModel Msg
  }

init =
  ( Model <| GameModel (1, 1) [] 100000 Nothing, Cmd.none )

update msg model =
  ( Model <| Game.update msg model.game, Cmd.none )

view model =
  svg [ width "600", height "500" ]
    <| renderGame model.game

subscriptions model =
  Keyboard.ups (\code -> PressesKey code)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
