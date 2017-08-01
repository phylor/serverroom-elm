module Support exposing (..)

import Infrastructure exposing (..)
import Random exposing (..)
import Staff exposing (..)

countSupportRequests model factor =
  floor <| factor * (toFloat <| numberOfClients model.infrastructure)

randomSupportRequests : Generator Float
randomSupportRequests =
  Random.float 0 0.2

updateSupportRequests model requests =
  { model | supportRequests = max 0 (model.supportRequests + requests - supportCasesHandledByStaff model) }

supportCasesHandledByStaff model =
  numberOfCasesHandledByASupportStaff * (List.length <| supportStaff model.infrastructure)
