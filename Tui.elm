module Tui exposing (Menu, MenuOption, renderWindow, renderMenu, menuMoveUp, menuMoveDown)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)

import Position exposing (Position)

type alias Menu messageType =
  { options : List (MenuOption messageType)
  , selectedOption : MenuOption messageType
  }

type alias MenuOption messageType =
  { title : String
  , action : messageType
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
    (List.map (\( option, ( xp, yp ) ) -> text_ [ x <| toString xp, y <| toString yp, menuOptionStyle option menu.selectedOption ] [ text option.title ]) (menuOptionsWithPosition menu.options xpos ypos))

menuOptionsWithPosition : List (MenuOption messageType) -> Int -> Int -> List ( MenuOption messageType, Position )
menuOptionsWithPosition options xpos ypos =
  let
    positions = List.map (\index -> ( xpos, ypos + index * 20 )) (List.range 1 <| List.length options)
  in
    List.map2 (\option position -> ( option, position )) options positions

menuOptionStyle option selectedOption =
  if option == selectedOption then
    Svg.Attributes.style "fill: rgb(224, 224, 246); font-family:sans-serif"
  else
    Svg.Attributes.style "fill: rgb(153, 151, 181); font-family:sans-serif"

menuMoveUp : Menu messageType -> Menu messageType
menuMoveUp menu =
  let
    indexedOptions = List.indexedMap (,) menu.options
    selectedOptionIndex = (case (List.head <| List.filter (\tuple -> second tuple == menu.selectedOption) indexedOptions) of
                                      Just tuple -> first tuple
                                      Nothing -> List.length menu.options)
    newOptionIndex = if 0 > selectedOptionIndex - 1 then selectedOptionIndex else selectedOptionIndex - 1
    newSelectedOption = (case (List.head <| List.filter (\tuple -> first tuple == newOptionIndex) indexedOptions) of
                          Just tuple -> second tuple
                          Nothing -> menu.selectedOption
                        )
  in
    { menu | selectedOption = newSelectedOption }

menuMoveDown : Menu messageType -> Menu messageType
menuMoveDown menu =
  let
    indexedOptions = List.indexedMap (,) menu.options
    selectedOptionIndex = (case (List.head <| List.filter (\tuple -> second tuple == menu.selectedOption) indexedOptions) of
                                      Just tuple -> first tuple
                                      Nothing -> -1)
    newOptionIndex = if List.length menu.options - 1 > selectedOptionIndex + 1 then selectedOptionIndex else selectedOptionIndex + 1
    newSelectedOption = (case (List.head <| List.filter (\tuple -> first tuple == newOptionIndex) indexedOptions) of
                          Just tuple -> second tuple
                          Nothing -> menu.selectedOption
                        )
  in
    { menu | selectedOption = newSelectedOption }
