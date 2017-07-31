module Game exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Date exposing (Date)

import Messages exposing (..)
import Position exposing (..)
import Infrastructure exposing (..)
import Dialog exposing (..)
import Player
import Grid
import Tui exposing (..)
import SimulationDay exposing (..)
import Staff exposing (..)
import Stats exposing (..)
import Financial exposing (..)

type alias Client =
  {
  }

type alias GameModel messageType =
  { playerPosition : Position
  , infrastructure : List Infrastructure
  , money : Int
  , dialog : Maybe (Dialog messageType)
  , date : Date
  }

styl =
  Svg.Attributes.style

renderBackground =
  rect [ width "500", height "520", styl "fill: rgb(200, 200, 200)" ] []

renderPlayer playerPosition =
  image [ x <| toString <| (toPixelX playerPosition) + 2, y <| toString <| (toPixelY playerPosition) + 2, width "45", height "45", xlinkHref "resources/engineer.svg" ] []

installDialog : GameModel Msg -> GameModel Msg
installDialog model =
  { model | dialog = Just <|
            Dialog "Which operating system do you want to install?"
              (Menu []
                ( MenuOption (operatingSystemLabel "Linux: $" costToBuildLinux) <| InstallLinux model.playerPosition )
                [ MenuOption (operatingSystemLabel "Windows: repeating $" repeatingCostsForWindows) (InstallWindows model.playerPosition)
                , MenuOption (operatingSystemLabel "Xen: $" costToBuildXen) (InstallXen model.playerPosition)
                ]
              )
  }

operatingSystemLabel : String -> Int -> String
operatingSystemLabel name costs =
  name ++ (toString costs)

movePlayerLeft model =
  { model | playerPosition = Player.moveLeft model.playerPosition }

movePlayerDown model =
  { model | playerPosition = Player.moveDown model.playerPosition }

movePlayerUp model =
  { model | playerPosition = Player.moveUp model.playerPosition }

movePlayerRight model =
  { model | playerPosition = Player.moveRight model.playerPosition }

renderGame model settings =
  [ renderBackground
  , renderWindow "[b]: build rack [w]: build workspace [i]: install/inspect [p]: pause" 0 0 500 20
  , Grid.render
  , renderInfrastructure model.infrastructure
  , renderPlayer model.playerPosition
  , renderDialog model
  , renderStats model
  ]

update msg model =
  case msg of
    PressesKey 37 ->
      movePlayerLeft model

    PressesKey 72 ->
      movePlayerLeft model

    PressesKey 38 ->
      case model.dialog of
        Just dialog ->
          let
            newDialog = { dialog | menu = menuMoveUp dialog.menu }
          in
            { model | dialog = Just newDialog }
        Nothing ->
          movePlayerUp model

    PressesKey 75 ->
      movePlayerUp model

    PressesKey 39 ->
      movePlayerRight model

    PressesKey 76 ->
      movePlayerRight model

    PressesKey 40 ->
      case model.dialog of
        Just dialog ->
          let
            newDialog = { dialog | menu = menuMoveDown dialog.menu }
          in
            { model | dialog = Just newDialog }
        Nothing ->
          movePlayerDown model

    PressesKey 74 ->
      movePlayerDown model

    PressesKey 66 ->
      if model.money >= costToBuildRack then
        case infrastructureAt model.infrastructure model.playerPosition of
          Just _ ->
            model
          Nothing ->
            { model | infrastructure = buildRack model.infrastructure model.playerPosition, money = model.money - costToBuildRack }
      else
        model

    PressesKey 73 ->
      case infrastructureAt model.infrastructure model.playerPosition of
        Just infra ->
          case infra of
            Rack info ->
              installDialog model
            Workplace info ->
              { model | infrastructure = hire Support model.infrastructure model.playerPosition }
        Nothing ->
          model

    PressesKey 13 -> -- Enter
      case model.dialog of
        Just dialog ->
          update dialog.menu.selectedOption.action model
        Nothing ->
          model

    PressesKey 87 -> -- w
      case infrastructureAt model.infrastructure model.playerPosition of
        Just _ ->
          model
        Nothing ->
          { model | infrastructure = buildWorkplace model.infrastructure model.playerPosition, money = model.money - 5000 }

    InstallLinux position ->
      { model | infrastructure = install Linux model.infrastructure position, dialog = Nothing }

    InstallWindows position ->
      { model | infrastructure = install Windows model.infrastructure position, dialog = Nothing }

    InstallXen position ->
      { model | infrastructure = install Xen model.infrastructure position, dialog = Nothing }

    ProceedToNextDay ->
      let
        newInfrastructure = addClient model.infrastructure
        newMoney = processMoney model
      in
        { model | date = nextDay model.date, infrastructure = newInfrastructure, money = newMoney }

    PressesKey _ ->
      model

    _ ->
      model
