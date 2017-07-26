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

type alias Client =
  {
  }

type alias GameModel messageType =
  { playerPosition : Position
  , infrastructure : List Infrastructure
  , money : Int
  , dialog : Maybe (Dialog messageType)
  , date : Date
  , clients : List Client
  }

renderStats model =
  g []
    [ rect [ x "500", y "0", width "100", height "20", Svg.Attributes.style "fill: rgb(0, 178, 181)" ] []
    , text_ [ x "505", y "15", Svg.Attributes.style "font-family: monospace; font-size: 0.75rem" ] [ text <| dateToString model.date ]
    , rect [ x "500", y "20", width "100", height "480", Svg.Attributes.style "fill: rgb(21, 3, 183)" ] []
    , line [ x1 "500", y1 "0", x2 "500", y2 "500", Svg.Attributes.style "stroke: rgb(224, 224, 246)" ] []

    , rect [ x "503", y "25", width "94", height "20", Svg.Attributes.style "stroke: rgb(224, 224, 246); fill: rgb(21, 3, 183)" ] []
    , line [ x1 "520", y1 "25", x2 "520", y2 "45", Svg.Attributes.style "stroke: rgb(224, 224, 246)" ] []
    , text_ [ x "505", y "40", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text "$" ]
    , text_ [ x "525", y "40", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text <| toString model.money]

    , rect [ x "503", y "45", width "94", height "20", Svg.Attributes.style "stroke: rgb(224, 224, 246); fill: rgb(21, 3, 183)" ] []
    , line [ x1 "520", y1 "45", x2 "520", y2 "65", Svg.Attributes.style "stroke: rgb(224, 224, 246)" ] []
    , text_ [ x "505", y "60", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text "S" ]
    , text_ [ x "525", y "60", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text <| toString <| List.length model.infrastructure ]

    , rect [ x "503", y "65", width "94", height "20", Svg.Attributes.style "stroke: rgb(224, 224, 246); fill: rgb(21, 3, 183)" ] []
    , line [ x1 "520", y1 "65", x2 "520", y2 "85", Svg.Attributes.style "stroke: rgb(224, 224, 246)" ] []
    , text_ [ x "505", y "80", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text "C" ]
    , text_ [ x "525", y "80", Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text <| toString <| List.length model.clients ]
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

renderGame model =
  [ renderBackground
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
      { model | infrastructure = buildWorkplace model.infrastructure model.playerPosition, money = model.money - 5000 }

    InstallLinux position ->
      { model | infrastructure = install Linux model.infrastructure position, dialog = Nothing }

    InstallWindows position ->
      { model | infrastructure = install Windows model.infrastructure position, dialog = Nothing }

    InstallXen position ->
      { model | infrastructure = install Xen model.infrastructure position, dialog = Nothing }

    ProceedToNextDay ->
      let
        newClients = processClients model
        newMoney = processMoney model
      in
        { model | date = nextDay model.date, clients = newClients, money = newMoney }

    PressesKey _ ->
      model

    _ ->
      model

processClients model =
  if 20 * List.length model.infrastructure > List.length model.clients then
    Client :: model.clients
  else
    model.clients

-- usually 1 rack = 42U
-- 1 rack of servers (hardware): $100'000
-- linux: $350 for installation -> $15'000 per rack
-- windows: $1500 per year -> $4 per day -> $175 per day per rack
-- xen: free, but only web hosting

-- 1 rack hardware: $100'000
-- 1 rack hardware: $500 per day
-- 1 rack linux: $15'000
-- 1 rack windows: $5'000 per day
-- 1 rack xen: $0

-- 1 rack linux: $1'200 per day
-- 1 rack windows: $20'000 per day
-- 1 rack xen: $800 per day
processMoney model =
  model.money - costs model + turnover model

costs model =
  List.sum <| List.map (\infrastructure -> repeatingCostsFor infrastructure) model.infrastructure

turnover model =
  List.sum <| List.map (\infrastructure -> turnoverFor infrastructure) model.infrastructure

repeatingCostsFor infrastructure =
  case infrastructure of
    Rack info ->
      repeatingCostForRack + repeatingCostForSystem info.system
    Workplace info ->
      500

repeatingCostForSystem system =
  case system of
    Just system ->
      case system of
        Linux ->
          0
        Windows ->
          repeatingCostsForWindows
        Xen ->
          0
    Nothing ->
      0

turnoverFor infrastructure =
  case infrastructure of
    Rack info ->
      case info.system of
        Just system ->
          case system of
            Linux ->
              1200
            Windows ->
              20000
            Xen ->
              800
        Nothing ->
          0
    Workplace info ->
      0
