module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Keyboard exposing (..)
import Char exposing (fromCode)
import Date exposing (Date)
import Time
import Random

import Messages exposing (..)
import Dialog exposing (..)
import Game exposing (..)
import Player exposing (moveLeft, moveUp, moveRight, moveDown)
import Tui exposing (..)
import GameOver exposing (..)
import Infrastructure exposing (..)
import Position exposing (..)
import Support exposing (..)

type State = MenuState
           | PlayingState
           | GameOverState
           | PauseState

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
  , activeDialog : Maybe (Dialog Msg)
  }

mainMenu =
  Menu [] (MenuOption "New Game" NewGame) [ MenuOption "Settings" SettingsMenu ]

settingsMenu =
  Menu [] (MenuOption "Graphics" GraphicsMenu) [ MenuOption "back" MainMenu ]

graphicsMenu settings =
  Menu [] (MenuOption ("Width: " ++ (toString <| settings.width)) ChangeWidth) [ MenuOption ("Height: " ++ (toString <| settings.height)) ChangeHeight, MenuOption "back" SettingsMenu ]

defaultSettings =
  Settings 640 520

startingDate : Date
startingDate =
  case Date.fromString "2003-01-01" of
    Ok date ->
      date
    Err message ->
      Date.fromTime 0

init =
  ( Model MenuState (Just mainMenu) initialGameModel defaultSettings Nothing Nothing, Cmd.none )

initialGameModel =
  (GameModel (1, 1) initialInfrastructure 500000 Nothing startingDate 0)

initialInfrastructure =
  [Doorway <| DoorwayInfo (5, 1) (6, 1)]

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    NewGame ->
      ( { model | activeUi = Nothing, state = PlayingState }, Cmd.none )

    MainMenu ->
      init

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
          case model.activeDialog of
            Just dialog ->
              let
                newDialog = { dialog | menu = menuMoveUp dialog.menu }
              in
                ( { model | activeDialog = Just newDialog }, Cmd.none )
            Nothing ->
              ( model, Cmd.none )

    MenuMoveDown ->
      case model.activeUi of
        Just menu ->
          ( { model | activeUi = Just <| menuMoveDown menu }, Cmd.none )
        Nothing ->
          case model.activeDialog of
            Just dialog ->
              let
                newDialog = { dialog | menu = menuMoveDown dialog.menu }
              in
                ( { model | activeDialog = Just newDialog }, Cmd.none )
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
              case model.activeDialog of
                Just dialog ->
                  update dialog.menu.selectedOption.action model
                Nothing ->
                  case model.state of
                    MenuState ->
                      ( model, Cmd.none )
                    PlayingState ->
                      ( { model | game = Game.update msg model.game }, Cmd.none )
                    GameOverState ->
                      ( model, Cmd.none )

                    PauseState ->
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

        GameOverState ->
          ( model, Cmd.none )
        PauseState ->
          case model.activeDialog of
            Just dialog ->
              update MenuMoveUp model
            Nothing ->
              ( model, Cmd.none )

    PressesKey 40 -> -- Arrow Down
      case model.state of
        MenuState ->
          case model.activeUi of
            Just ui ->
              update MenuMoveDown model
            Nothing ->
              case model.activeDialog of
                Just dialog ->
                  update MenuMoveDown model
                Nothing ->
                  ( model, Cmd.none )

        PlayingState ->
          ( { model | game = Game.update msg model.game }, Cmd.none )
        GameOverState ->
          ( model, Cmd.none )

        PauseState ->
          case model.activeDialog of
            Just dialog ->
              update MenuMoveDown model
            Nothing ->
              ( model, Cmd.none )

    Tick time ->
      let
        updated = update ProceedToNextDay model
        newModel = first updated
      in
        if isGameOver newModel then
          ( { newModel | state = GameOverState }, second updated )
        else
          ( newModel, Random.generate RandomSupportRequests randomSupportRequests ) 

    PressesKey 80 -> -- P
      if model.state == PlayingState then
        ( { model | activeDialog = Just <| pauseDialog, state = PauseState }, Cmd.none )
      else
        ( model, Cmd.none )

    PressesKey 27 -> -- Esc
      if model.state == PlayingState then
        ( { model | activeDialog = Just <| pauseDialog, state = PauseState }, Cmd.none )
      else
        ( model, Cmd.none )

    Unpause ->
       ( { model | activeDialog = Nothing, state = PlayingState }, Cmd.none )

    RandomSupportRequests requestsFactor ->
      ( { model | game = updateSupportRequests model.game (countSupportRequests model.game requestsFactor) }, Cmd.none )

    _ ->
      case model.state of
        MenuState ->
          ( model, Cmd.none )
        PlayingState ->
          ( { model | game = Game.update msg model.game }, Cmd.none )
        GameOverState ->
          ( model, Cmd.none )

        PauseState ->
          ( model, Cmd.none )

pauseDialog =
  Dialog "Game paused" <|
    Menu
      [ MenuOption "Exit to Main Menu" MainMenu]
      (MenuOption "Continue" Unpause)
      []

view : Model -> Html.Html Msg
view model =
  svg [ width <| toString <| model.settings.width, height <| toString <| model.settings.height ]
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
        renderGame model.game model.settings
      GameOverState ->
        [renderGameOver model.settings]
      PauseState ->
        case model.activeDialog of
          Just dialog ->
            [ (g [] <| renderGame model.game model.settings)
            , renderDialogAt dialog
            ]

          Nothing ->
            []
    )

subscriptions model =
  Sub.batch
    [ Keyboard.ups (\code -> PressesKey code)
    , Time.every (2 * Time.second) Tick
    ]

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
