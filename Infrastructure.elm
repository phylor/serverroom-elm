module Infrastructure exposing (..)

import Position exposing (..)

type alias Infrastructure =
  { object : InfrastructureType
  , position : Position
  , system : Maybe OperatingSystem
  }

type OperatingSystem = Linux
                     | Windows

type InfrastructureType = Server

installLinux : List Infrastructure -> Position -> List Infrastructure
installLinux infrastructure position =
  List.map (\item -> if item.position == position then { item | system = Just Linux } else item) infrastructure

installWindows : List Infrastructure -> Position -> List Infrastructure
installWindows infrastructure position =
  List.map (\item -> if item.position == position then { item | system = Just Windows } else item) infrastructure

buildServer : List Infrastructure -> Position -> Maybe OperatingSystem -> List Infrastructure
buildServer infrastructure position system =
  Infrastructure Server position system :: infrastructure

