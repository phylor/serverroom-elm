module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Keyboard exposing (..)
import Char exposing (fromCode)

import Grid exposing (render)
import Player exposing (moveLeft, moveUp, moveRight, moveDown)

type Msg = PressesKey Keyboard.KeyCode -- for KeyCodes check https://www.w3.org/2002/09/tests/keys.html

type alias Position =
  ( Int, Int )

type alias Infrastructure =
  { object : InfrastructureType
  , position : Position
  }

type InfrastructureType = Server

type alias Model =
  { playerPosition : Position
  , infrastructure : List Infrastructure
  }

init =
  ( Model (1, 1) [], Cmd.none )

update msg model =
  case msg of
    PressesKey 37 ->
      ( { model | playerPosition = Player.moveLeft model.playerPosition }, Cmd.none )

    PressesKey 38 ->
      ( { model | playerPosition = Player.moveUp model.playerPosition }, Cmd.none )

    PressesKey 39 ->
      ( { model | playerPosition = Player.moveRight model.playerPosition }, Cmd.none )

    PressesKey 40 ->
      ( { model | playerPosition = Player.moveDown model.playerPosition }, Cmd.none )

    PressesKey 66 ->
      ( { model | infrastructure = buildServer model.infrastructure model.playerPosition }, Cmd.none )

    PressesKey _ ->
      ( model, Cmd.none )

buildServer : List Infrastructure -> Position -> List Infrastructure
buildServer infrastructure position =
  Infrastructure Server position :: infrastructure

view model =
  svg [ width "500", height "500" ]
    [ renderBackground
    , Grid.render
    , renderPlayer model.playerPosition
    , renderInfrastructure model.infrastructure
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

renderInfrastructure infrastructure =
  g []
    (List.map (\item -> image [ x <| toString <| (first item.position - 1) * 50, y <| toString <| (second item.position - 1) * 50, width "45", height "45", xlinkHref "resources/server.svg" ] []) infrastructure)

subscriptions model =
  Keyboard.ups (\code -> PressesKey code)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
