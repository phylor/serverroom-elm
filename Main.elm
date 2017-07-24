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

init =
  ( Model MenuState (Just (Menu [ MenuOption "New Game" NewGame ] ( MenuOption "New Game" NewGame ))) <| GameModel (1, 1) [] 100000 Nothing, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      ( { model | state = PlayingState }, Cmd.none )

    PressesKey 13 ->
      case model.activeUi of
        Just ui ->
          update ui.selectedOption.action { model | activeUi = Nothing }
        Nothing ->
          ( model, Cmd.none )

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
        [ createWindow "Server Room" 0 0 600 500
        , createMenu [ MenuOption "New Game" NewGame ]
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
