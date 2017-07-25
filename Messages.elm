module Messages exposing (..)

import Keyboard

import Position exposing (Position)

type Msg = PressesKey Keyboard.KeyCode -- for KeyCodes check https://www.w3.org/2002/09/tests/keys.html
         | InstallLinux Position
         | InstallWindows Position
         | NewGame
         | MenuMoveUp
         | MenuMoveDown
         | MainMenu
         | SettingsMenu
