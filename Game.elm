module Game exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)

import Messages exposing (..)
import Position exposing (..)
import Infrastructure exposing (..)
import Dialog exposing (..)
import Player
import Grid

type alias GameModel messageType =
  { playerPosition : Position
  , infrastructure : List Infrastructure
  , money : Int
  , dialog : Maybe (Dialog messageType)
  }

renderStats model =
  g []
    [ rect [ x "500", y "0", width "100", height "20", Svg.Attributes.style "fill: rgb(0, 178, 181)" ] []
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
    ]

renderInfrastructure infrastructure =
  g []
    (List.map (\item ->
      case item.system of
        Just Linux ->
          g []
            [ renderServer item.position
            , renderLinux item.position
            ]
        Just Windows ->
          g []
            [ renderServer item.position
            , renderWindows item.position
            ]
        Nothing ->
          renderServer item.position
    ) infrastructure)

renderServer position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/server.svg" ] []

renderLinux position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/linux.svg" ] []

renderWindows position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/windows.svg" ] []

renderDialog model =
  case model.dialog of
    Just dialog ->
      renderDialogAt dialog
    Nothing ->
      g [] []

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
  { model | dialog = Just <| Dialog "Which operating system do you want to install? 1: Linux 2: Windows" [ DialogOption "Linux" (InstallLinux model.playerPosition), DialogOption "Windows" (InstallWindows model.playerPosition) ] }

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
        { model | infrastructure = buildServer model.infrastructure model.playerPosition Nothing, money = model.money - 20000 }
      else
        model

    PressesKey 73 ->
      installDialog model

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
                model
        Nothing ->
          model

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
                model
        Nothing ->
          model

    InstallLinux position ->
      { model | infrastructure = installLinux model.infrastructure position, dialog = Nothing }

    InstallWindows position ->
      { model | infrastructure = installWindows model.infrastructure position, dialog = Nothing }

    PressesKey _ ->
      model
