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

type alias Settings =
  { width : Int
  , height : Int
  }

type alias Model =
  { state : State
  , activeUi : Maybe (Menu Msg)
  , game : GameModel Msg
  , settings : Settings
  , activeForm : Maybe (Form Msg)
  }

mainMenu =
  Menu [] (MenuOption "New Game" NewGame) [ MenuOption "Settings" SettingsMenu ]

settingsMenu =
  Menu [] (MenuOption "Graphics" GraphicsMenu) [ MenuOption "back" MainMenu ]

graphicsMenu settings =
  Menu [] (MenuOption ("Width: " ++ (toString <| settings.width)) ChangeWidth) [ MenuOption ("Height: " ++ (toString <| settings.height)) ChangeHeight, MenuOption "back" SettingsMenu ]

defaultSettings =
  Settings 640 480

init =
  ( Model MenuState (Just mainMenu) (GameModel (1, 1) [] 100000 Nothing) defaultSettings Nothing, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      ( { model | state = PlayingState }, Cmd.none )

    MainMenu ->
      ( { model | activeUi = Just mainMenu }, Cmd.none )

    SettingsMenu ->
      ( { model | activeUi = Just settingsMenu }, Cmd.none )

    GraphicsMenu ->
      ( { model | activeUi = Just <| graphicsMenu model.settings }, Cmd.none )

    ChangeWidth ->
      ( { model | activeUi = Nothing, activeForm = Just <| Form "" (\input -> SaveWidth input) }, Cmd.none )

    ChangeHeight ->
      ( { model | activeUi = Nothing, activeForm = Just <| Form "" (\input -> SaveHeight input) }, Cmd.none )

    SaveWidth width ->
      let
        newSettings = (case String.toInt width of
                    Err message -> Settings model.settings.width model.settings.height
                    Ok newWidth -> Settings newWidth model.settings.height
                   )
      in
        ( { model | settings = newSettings, activeUi = Just <| graphicsMenu <| newSettings }, Cmd.none )

    SaveHeight height ->
      let
        settings = model.settings
        newSettings = (case String.toInt height of
                        Err message -> { settings | height = model.settings.height }
                        Ok newHeight -> { settings | height = newHeight }
                      )
      in
        ( { model | settings = newSettings, activeUi = Just <| graphicsMenu <| newSettings }, Cmd.none )

    UserFormInput input ->
      ( { model | activeForm = updateForm model.activeForm input }, Cmd.none )

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
          case model.activeForm of
            Just form ->
              update (form.action form.currentValue) { model | activeForm = Nothing }
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
  svg [ width <| toString <| model.settings.width, height "500" ]
    (case model.state of
      MenuState ->
        [ renderWindow "Server Room" 0 0 model.settings.width 500
        , (case model.activeUi of
            Just menu ->
              renderMenu menu 200 200
            Nothing ->
              g [] []
          )
        , (case model.activeForm of
            Just form ->
              renderForm form UserFormInput
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
