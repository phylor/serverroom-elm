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

init =
  ( GameModel (1, 1) [] 100000 Nothing, Cmd.none )

update msg model =
  case msg of
    PressesKey 37 ->
      movePlayerLeft model

    PressesKey 72 ->
      movePlayerLeft model

    PressesKey 38 ->
      movePlayerUp model

    PressesKey 75 ->
      movePlayerUp model

    PressesKey 39 ->
      movePlayerRight model

    PressesKey 76 ->
      movePlayerRight model

    PressesKey 40 ->
      movePlayerDown model

    PressesKey 74 ->
      movePlayerDown model

    PressesKey 66 ->
      if model.money >= 20000 then
        ( { model | infrastructure = buildServer model.infrastructure model.playerPosition Nothing, money = model.money - 20000 }, Cmd.none )
      else
        ( model, Cmd.none )

    PressesKey 73 ->
      ( installDialog model, Cmd.none )

    PressesKey 49 -> -- 1
      case model.dialog of
        Just dialog ->
          let
            option = List.head dialog.options
          in
            case option of
              Just opt ->
                update opt.action model
              Nothing ->
                ( model, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    PressesKey 50 -> -- 2
      case model.dialog of
        Just dialog ->
          let
            option = List.head <| List.drop 1 dialog.options
          in
            case option of
              Just opt ->
                update opt.action model
              Nothing ->
                ( model, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    InstallLinux position ->
      ( { model | infrastructure = installLinux model.infrastructure position, dialog = Nothing }, Cmd.none )

    InstallWindows position ->
      ( { model | infrastructure = installWindows model.infrastructure position, dialog = Nothing }, Cmd.none )

    PressesKey _ ->
      ( model, Cmd.none )

view model =
  svg [ width "600", height "500" ]
    [ renderBackground
    , Grid.render
    , renderInfrastructure model.infrastructure
    , renderPlayer model.playerPosition
    , renderDialog model
    , renderStats model
    ]

subscriptions model =
  Keyboard.ups (\code -> PressesKey code)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
