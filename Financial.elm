module Financial exposing (..)

import Infrastructure exposing (..)

-- usually 1 rack = 42U
-- 1 rack of servers (hardware): $100'000
-- linux: $350 for installation -> $15'000 per rack
-- windows: $1500 per year -> $4 per day -> $175 per day per rack
-- xen: free, but only web hosting

-- 1 rack hardware: $100'000
-- 1 rack hardware: $500 per day
-- 1 rack linux: $15'000
-- 1 rack windows: $5'000 per day
-- 1 rack xen: $0

-- 1 rack linux: $1'200 per day
-- 1 rack windows: $20'000 per day
-- 1 rack xen: $800 per day
processMoney model =
  model.money - costs model + turnover model

costs model =
  List.sum <| List.map (\infrastructure -> repeatingCostsFor infrastructure) model.infrastructure

turnover model =
  List.sum <| List.map (\infrastructure -> turnoverFor infrastructure) model.infrastructure

repeatingCostsFor infrastructure =
  case infrastructure of
    Rack info ->
      repeatingCostForRack + repeatingCostForSystem info.system
    Workplace info ->
      500

repeatingCostForSystem system =
  case system of
    Just system ->
      case system of
        Linux ->
          0
        Windows ->
          repeatingCostsForWindows
        Xen ->
          0
    Nothing ->
      0

turnoverFor infrastructure =
  case infrastructure of
    Rack info ->
      case info.system of
        Just system ->
          case system of
            Linux ->
              info.client * 25
            Windows ->
              info.client * 480
            Xen ->
              info.client * 20
        Nothing ->
          0
    Workplace info ->
      0

numberOfRacks infrastructure =
  List.length <| racks infrastructure

numberOfWorkplaces infrastructure =
  List.length <| workplaces infrastructure

numberOfSupportStaff infrastructure =
  List.length <| supportStaff infrastructure 
