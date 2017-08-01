module Messages exposing (..)

import Keyboard
import Time exposing (Time)

import Position exposing (Position)

type Msg = PressesKey Keyboard.KeyCode -- for KeyCodes check https://www.w3.org/2002/09/tests/keys.html
         | InstallLinux Position
         | InstallWindows Position
         | InstallXen Position
         | NewGame
         | MenuMoveUp
         | MenuMoveDown
         | MainMenu
         | SettingsMenu
         | ChangeWidth
         | GraphicsMenu
         | UserFormInput String
         | SaveWidth String
         | ChangeHeight
         | SaveHeight String
         | ProceedToNextDay
         | Tick Time
         | Unpause
         | CancelDialog
