module Infrastructure exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Position exposing (..)

type Infrastructure = Rack RackInfo
                    | Workplace WorkplaceInfo

type alias RackInfo =
  { position : Position
  , system : Maybe OperatingSystem
  }

type alias WorkplaceInfo =
  { position : Position
  , staff : Maybe Staff
  }

type Staff = Support

type OperatingSystem = Linux
                     | Windows
                     | Xen

install : OperatingSystem -> List Infrastructure -> Position -> List Infrastructure
install system infrastructure position =
  List.map (\item -> 
            case item of
              Rack info ->
                if info.position == position then
                  Rack { info | system = Just system }
                else
                  Rack info
              Workplace info ->
                Workplace info
           ) infrastructure

buildRack : List Infrastructure -> Position -> List Infrastructure
buildRack infrastructure position =
  (Rack <| RackInfo position Nothing) :: infrastructure

buildWorkplace infrastructure position =
  (Workplace <| WorkplaceInfo position Nothing) :: infrastructure

renderDoorway =
  image [ x <| toPixelX (5,1), y <| toPixelY (5,1), width "100", height "50", xlinkHref "resources/doorway.svg" ] []

repeatingCostForRack =
  500

costToBuildRack =
  100000

costToBuildLinux =
  15000

turnoverLinux =
  1200

turnoverWindows =
  20000

turnoverXen =
  800

renderInfrastructure infrastructure =
  g []
    [ renderDoorway
    , g []
      (List.map renderInfra infrastructure)
    ]

renderInfra infrastructure =
  case infrastructure of
    Rack info ->
      renderRack info
    Workplace info ->
      renderWorkplace info

renderWorkplace item =
  image [ x <| toPixelX item.position, y <| toPixelY item.position, width "50", height "50", xlinkHref "resources/workspace.svg" ] []

renderRack infrastructure =
  case infrastructure.system of
    Just Linux ->
      g []
        [ renderServer infrastructure.position
        , renderLinux infrastructure.position
        ]
    Just Windows ->
      g []
        [ renderServer infrastructure.position
        , renderWindows infrastructure.position
        ]
    Just Xen ->
      g []
        [ renderServer infrastructure.position
        , renderXen infrastructure.position
        ]
    Nothing ->
      renderServer infrastructure.position

renderServer position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/server.svg" ] []

renderLinux position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/linux.svg" ] []

renderWindows position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/windows.svg" ] []

renderXen position =
  image [ x <| toPixelX position, y <| toPixelY position, width "50", height "50", xlinkHref "resources/xen.svg" ] []

