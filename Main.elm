module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Keyboard exposing (..)
import Char exposing (fromCode)

import Grid exposing (render)
import Player exposing (moveLeft, moveUp, moveRight, moveDown)

type Msg = PressesKey Keyboard.KeyCode -- for KeyCodes check https://www.w3.org/2002/09/tests/keys.html
         | InstallLinux Position
         | InstallWindows Position

type alias Position =
  ( Int, Int )

type alias Infrastructure =
  { object : InfrastructureType
  , position : Position
  , system : Maybe OperatingSystem
  }

type OperatingSystem = Linux
                     | Windows

type InfrastructureType = Server

type alias Dialog =
  { message : String
  , options : List DialogOption
  }

type alias DialogOption =
  { title : String
  , action : Msg
  }

type alias Model =
  { playerPosition : Position
  , infrastructure : List Infrastructure
  , money : Int
  , dialog : Maybe Dialog
  }

init =
  ( Model (1, 1) [] 100000 Nothing, Cmd.none )

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

installLinux : List Infrastructure -> Position -> List Infrastructure
installLinux infrastructure position =
  List.map (\item -> if item.position == position then { item | system = Just Linux } else item) infrastructure

installWindows : List Infrastructure -> Position -> List Infrastructure
installWindows infrastructure position =
  List.map (\item -> if item.position == position then { item | system = Just Windows } else item) infrastructure

installDialog : Model -> Model
installDialog model =
  { model | dialog = Just <| Dialog "Which operating system do you want to install? 1: Linux 2: Windows" [ DialogOption "Linux" (InstallLinux model.playerPosition), DialogOption "Windows" (InstallWindows model.playerPosition) ] }

movePlayerLeft model =
  ( { model | playerPosition = Player.moveLeft model.playerPosition }, Cmd.none )

movePlayerDown model =
  ( { model | playerPosition = Player.moveDown model.playerPosition }, Cmd.none )

movePlayerUp model =
  ( { model | playerPosition = Player.moveUp model.playerPosition }, Cmd.none )

movePlayerRight model =
  ( { model | playerPosition = Player.moveRight model.playerPosition }, Cmd.none )

buildServer : List Infrastructure -> Position -> Maybe OperatingSystem -> List Infrastructure
buildServer infrastructure position system =
  Infrastructure Server position system :: infrastructure

view model =
  svg [ width "600", height "500" ]
    [ renderBackground
    , Grid.render
    , renderInfrastructure model.infrastructure
    , renderPlayer model.playerPosition
    , renderMoney model.money
    , renderDialog model
    ]

styl =
  Svg.Attributes.style

renderDialog model =
  case model.dialog of
    Just dialog ->
      g []
        [ rect [ x "50", y "100", width "400", height "300", styl "fill: rgb(21, 3, 183)" ] []
        , text_ [ x "75", y "125", styl "fill: rgb(144, 144, 181); font-family: sans-serif" ] [ text dialog.message ]
        ]
-- 144 144 181

    Nothing ->
      g [] []

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

toPixelX : Position -> String
toPixelX position =
  toString <| (first position - 1) * 50

toPixelY : Position -> String
toPixelY position =
  toString <| (second position - 1) * 50

renderServer position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/server.svg" ] []

renderLinux position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/linux.svg" ] []

renderWindows position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/windows.svg" ] []

renderMoney money =
  text_ [ x "525", y "25" ] [ text <| String.cons '$' <| toString money]

subscriptions model =
  Keyboard.ups (\code -> PressesKey code)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
