module Staff exposing (..)

import Infrastructure exposing (..)
import Position exposing (..)

hire : Staff -> List Infrastructure -> Position -> List Infrastructure
hire staffType infrastructure position =
  let
    workplace = infrastructureAt infrastructure position
    newWorkplace = case workplace of
                     Just infra ->
                       case infra of
                         Rack info ->
                           Nothing
                         Workplace info ->
                           Just <| Workplace <| WorkplaceInfo position <| Just staffType
                     Nothing ->
                       Nothing
  in
    case newWorkplace of
      Just workplace ->
        updateInfrastructure infrastructure workplace
      Nothing ->
        infrastructure
