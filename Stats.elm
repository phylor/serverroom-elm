module Stats exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)

import SimulationDay exposing (..)
import Tui exposing (..)
import Financial exposing (..)

renderStats model =
  g []
    [ renderWindow (dateToString model.date) 500 0 100 500

    , g []
        (renderAttributes
          [ ("Funds", model.money)
          , ("Costs", costs model)
          , ("Turnover", turnover model)
          , ("Racks", numberOfRacks model.infrastructure)
          , ("Workplaces", numberOfWorkplaces model.infrastructure)
          , ("Support staff", numberOfSupportStaff model.infrastructure)
          ]
        )
    ]

renderAttributes attributes =
  List.indexedMap renderAttribute attributes

renderAttribute index attribute =
  g []
    [ text_ [ x "505", y <| toString (36 * (index+1)), Svg.Attributes.style "fill: rgb(224, 224, 246); font-family: monospace; font-size: 0.75rem" ] [ text <| first attribute ]
    , text_ [ x "505", y <| toString (36 * (index+1) + 16), Svg.Attributes.style "fill: rgb(224, 224, 246)" ] [ text <| toString <| second attribute ]
    ]
