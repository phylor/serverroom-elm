module Infrastructure exposing (..)

import Position exposing (..)

type alias Infrastructure =
  { object : InfrastructureType
  , position : Position
  , system : Maybe OperatingSystem
  }

type OperatingSystem = Linux
                     | Windows
                     | Xen

type InfrastructureType = Server

install system infrastructure position =
  List.map (\item -> if item.position == position then { item | system = Just system } else item) infrastructure

buildServer : List Infrastructure -> Position -> Maybe OperatingSystem -> List Infrastructure
buildServer infrastructure position system =
  Infrastructure Server position system :: infrastructure

