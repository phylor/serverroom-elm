module SimulationDay exposing (dateToString, nextDay)

import Date exposing (Date)

dayInMilliseconds =
  1000 * 60 * 60 * 24

nextDay : Date -> Date
nextDay date =
  Date.fromTime <| Date.toTime date + dayInMilliseconds

dateToString date =
  List.foldr String.append "" <| List.intersperse " " [ toString <| Date.dayOfWeek date
                                                      , dateDay date
                                                      , toString <| Date.month date
                                                      , toString <| Date.year date
                                                      ]

dateDay date =
  let
    dayString = toString <| Date.day date
  in
    if String.length dayString <= 1 then
      "0" ++ dayString
    else
      dayString

