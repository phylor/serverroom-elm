module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Keyboard exposing (..)
import Char exposing (fromCode)

import Messages exposing (..)
import Dialog exposing (..)
import Game exposing (..)
import Player exposing (moveLeft, moveUp, moveRight, moveDown)
import Tui exposing (..)

type State = MenuState
           | PlayingState

type alias Model =
  { state : State
  , activeUi : Maybe (Menu Msg)
  , game : GameModel Msg
  }

mainMenu =
  Menu [ MenuOption "New Game" NewGame, MenuOption "Settings" SettingsMenu ] ( MenuOption "New Game" NewGame )

settingsMenu =
  Menu [ MenuOption "Graphics" MainMenu, MenuOption "back" MainMenu ] ( MenuOption "Graphics" MainMenu )

init =
  ( Model MenuState (Just mainMenu) <| GameModel (1, 1) [] 100000 Nothing, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      ( { model | state = PlayingState }, Cmd.none )

    MainMenu ->
      ( { model | activeUi = Just mainMenu }, Cmd.none )

    SettingsMenu ->
      ( { model | activeUi = Just settingsMenu }, Cmd.none )

    MenuMoveUp ->
      case model.activeUi of
        Just menu ->
          ( { model | activeUi = Just <| menuMoveUp menu }, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    MenuMoveDown ->
      case model.activeUi of
        Just menu ->
          ( { model | activeUi = Just <| menuMoveDown menu }, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    PressesKey 13 -> -- Enter
      case model.activeUi of
        Just ui ->
          update ui.selectedOption.action { model | activeUi = Nothing }
        Nothing ->
          ( model, Cmd.none )

    PressesKey 38 -> -- Arrow Up
      case model.state of
        MenuState ->
          case model.activeUi of
            Just ui ->
              update MenuMoveUp model
            Nothing ->
              ( model, Cmd.none )

        PlayingState ->
          ( { model | game = Game.update msg model.game }, Cmd.none )

    PressesKey 40 -> -- Arrow Down
      case model.state of
        MenuState ->
          case model.activeUi of
            Just ui ->
              update MenuMoveDown model
            Nothing ->
              ( model, Cmd.none )

        PlayingState ->
          ( { model | game = Game.update msg model.game }, Cmd.none )

    _ ->
      case model.state of
        MenuState ->
          ( model, Cmd.none )
        PlayingState ->
          ( { model | game = Game.update msg model.game }, Cmd.none )

view model =
  svg [ width "600", height "500" ]
    (case model.state of
      MenuState ->
        [ renderWindow "Server Room" 0 0 600 500
        , (case model.activeUi of
            Just menu ->
              renderMenu menu 200 200
            Nothing ->
              g [] []
          )
        ]
      PlayingState ->
        renderGame model.game
    )

subscriptions model =
  Keyboard.ups (\code -> PressesKey code)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
