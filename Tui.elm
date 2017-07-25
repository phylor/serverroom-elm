module Tui exposing (Menu, MenuOption, Form, renderWindow, renderMenu, menuMoveUp, menuMoveDown, renderForm, updateForm)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import Html exposing (input)
import Html.Events exposing (onInput)

import Position exposing (Position)

type alias Menu messageType =
  { beforeSelectedOption : List (MenuOption messageType)
  , selectedOption : MenuOption messageType
  , afterSelectedOption : List (MenuOption messageType)
  }

type alias MenuOption messageType =
  { title : String
  , action : messageType
  }

type alias Form actionType =
  { currentValue : String
  , action : String -> actionType
  }

renderWindow : String -> Int -> Int -> Int -> Int -> Svg message
renderWindow title xpos ypos w h =
  g []
    [ rect [ x <| toString xpos, y <| toString ypos, width <| toString w, height <| toString h, Svg.Attributes.style "fill: rgb(21, 3, 183)" ] []
    , rect [ x <| toString xpos, y <| toString ypos, width <| toString w, height "20", Svg.Attributes.style "fill: rgb(0, 178, 181)" ] []
    , text_ [ x <| toString (xpos + 3), y <| toString (ypos + 15), Svg.Attributes.style "font-family:sans-serif" ] [ text title ]
    ]

renderMenu : Menu messageType -> Int -> Int -> Svg message
renderMenu menu xpos ypos =
  g []
    (List.map (\( option, ( xp, yp ) ) -> (if option == menu.selectedOption then renderSelectedMenuOption option xp yp else renderMenuOption option xp yp)) (menuOptionsWithPosition (menuOptions menu) xpos ypos))

menuOptionsWithPosition : List (MenuOption messageType) -> Int -> Int -> List ( MenuOption messageType, Position )
menuOptionsWithPosition options xpos ypos =
  let
    positions = List.map (\index -> ( xpos, ypos + index * 20 )) (List.range 1 <| List.length options)
  in
    List.map2 (\option position -> ( option, position )) options positions

renderMenuOption option xpos ypos =
  text_ [ x <| toString xpos, y <| toString ypos, Svg.Attributes.style "fill: rgb(153, 151, 181); font-family:sans-serif" ] [ text option.title ]

renderSelectedMenuOption option xpos ypos =
  g []
    [ rect [ x <| toString (xpos - 3), y <| toString (ypos - 15), width "200", height "20", Svg.Attributes.style "fill: rgb(153, 151, 181)" ] []
    , text_ [ x <| toString xpos, y <| toString ypos, Svg.Attributes.style "fill: rgb(21, 3, 183); font-family:sans-serif" ] [ text option.title ]
    ]

menuMoveUp : Menu messageType -> Menu messageType
menuMoveUp menu =
  case List.reverse menu.beforeSelectedOption |> List.head of
    Just newSelectedOption ->
      let
        newAfterSelectedOption = menu.selectedOption :: menu.afterSelectedOption
        newBeforeSelectedOption = List.reverse menu.beforeSelectedOption |> List.drop 1 |> List.reverse
      in
        Menu newBeforeSelectedOption newSelectedOption newAfterSelectedOption
    Nothing ->
      menu

menuMoveDown : Menu messageType -> Menu messageType
menuMoveDown menu =
  case List.head menu.afterSelectedOption of
    Just newSelectedOption ->
      let
        newAfterSelectedOption = List.drop 1 menu.afterSelectedOption
        newBeforeSelectedOption = List.append menu.beforeSelectedOption [menu.selectedOption]
      in
        Menu newBeforeSelectedOption newSelectedOption newAfterSelectedOption
    Nothing ->
      menu

menuOptions menu =
  List.concat [menu.beforeSelectedOption, [menu.selectedOption], menu.afterSelectedOption]

renderForm : (Form messageType) -> (String -> messageType) -> Svg messageType
renderForm form action =
  g []
    [ foreignObject [ x "200", y "200", width "200", height "200" ]
                    [ input [ onInput action ] []
                    ]
    ]

updateForm : Maybe (Form messageType) -> String -> Maybe (Form messageType)
updateForm form input =
  case form of
    Just f ->
      Just { f | currentValue = input }
    Nothing ->
      Nothing
