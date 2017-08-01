module Infrastructure exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Position exposing (..)

type Infrastructure = Rack RackInfo
                    | Workplace WorkplaceInfo
                    | Doorway DoorwayInfo

type alias RackInfo =
  { position : Position
  , system : Maybe OperatingSystem
  , client : Int
  }

type alias WorkplaceInfo =
  { position : Position
  , staff : Maybe Staff
  }

type alias DoorwayInfo =
  { positionLeft : Position
  , positionRight : Position
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
              Doorway info ->
                Doorway info
           ) infrastructure

buildRack : List Infrastructure -> Position -> List Infrastructure
buildRack infrastructure position =
  (Rack <| RackInfo position Nothing 0) :: infrastructure

buildWorkplace infrastructure position =
  (Workplace <| WorkplaceInfo position Nothing) :: infrastructure

renderDoorway info =
  image [ x <| toPixelXString info.positionLeft, y <| toPixelYString info.positionLeft, width "100", height "50", xlinkHref "resources/doorway.svg" ] []

repeatingCostForRack =
  500

costToBuildRack =
  100000

costToBuildLinux =
  15000

costToBuildXen =
  0

repeatingCostsForWindows =
  5000

turnoverLinux =
  1200

turnoverWindows =
  20000

turnoverXen =
  800

renderInfrastructure infrastructure =
  g []
    (List.map renderInfra infrastructure)

renderInfra infrastructure =
  case infrastructure of
    Rack info ->
      renderRack info
    Workplace info ->
      renderWorkplace info
    Doorway info ->
      renderDoorway info

renderWorkplace workplaceInfo =
  case workplaceInfo.staff of
    Just staff ->
      g []
        [ image [ x <| toPixelXString workplaceInfo.position, y <| toPixelYString workplaceInfo.position, width "50", height "50", xlinkHref "resources/workspace.svg" ] []
        , image [ x <| toPixelXString workplaceInfo.position, y <| toPixelYString workplaceInfo.position, width "50", height "50", xlinkHref "resources/support.svg" ] []
        ]
    Nothing ->
      image [ x <| toPixelXString workplaceInfo.position, y <| toPixelYString workplaceInfo.position, width "50", height "50", xlinkHref "resources/workspace.svg" ] []

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
  image [ x <| toPixelXString position, y <| toPixelYString position, width "50", height "50", xlinkHref "resources/server.svg" ] []

renderLinux position =
  image [ x <| toPixelXString position, y <| toPixelYString position, width "50", height "50", xlinkHref "resources/linux.svg" ] []

renderWindows position =
  image [ x <| toPixelXString position, y <| toPixelYString position, width "50", height "50", xlinkHref "resources/windows.svg" ] []

renderXen position =
  image [ x <| toPixelXString position, y <| toPixelYString position, width "50", height "50", xlinkHref "resources/xen.svg" ] []

infrastructureAt : List Infrastructure -> Position -> Maybe Infrastructure
infrastructureAt infrastructure position =
  List.head <| List.filter (infrastructureFilter position) infrastructure

positionOf : Infrastructure -> Position
positionOf infrastructure =
  case infrastructure of
    Rack info ->
      info.position
    Workplace info ->
      info.position
    Doorway info ->
      info.positionLeft

infrastructureFilter : Position -> Infrastructure -> Bool
infrastructureFilter position infrastructure =
  case infrastructure of
    Rack info ->
      info.position == position
    Workplace info ->
      info.position == position
    Doorway info ->
      info.positionLeft == position || info.positionRight == position

updateInfrastructure : List Infrastructure -> Infrastructure -> List Infrastructure
updateInfrastructure infrastructure updatedInfrastructure =
  let
    position = case updatedInfrastructure of
                 Rack info ->
                   info.position
                 Workplace info ->
                   info.position
                 Doorway info ->
                   info.positionLeft
  in
    List.map (\item -> case item of 
                         Rack info ->
                           if info.position == position then
                             updatedInfrastructure
                           else
                             Rack info
                         Workplace info ->
                           if info.position == position then
                             updatedInfrastructure
                           else
                             Workplace info
                         Doorway info ->
                           Doorway info
             ) infrastructure

racks infrastructure =
  List.filter isRack infrastructure

isRack infrastructure =
  case infrastructure of
    Rack _ ->
      True
    Workplace _ ->
      False
    Doorway _ ->
      False


workplaces infrastructure =
  List.filter isWorkplace infrastructure

isWorkplace infrastructure =
  case infrastructure of
    Rack _ ->
      False
    Workplace _ ->
      True
    Doorway _ ->
      False

supportStaff infrastructure =
  List.filter hasSupportStaff <| workplaces infrastructure

hasSupportStaff infrastructure =
  case infrastructure of
    Rack _ ->
      False
    Workplace info ->
      info.staff == Just Support
    Doorway info ->
      False

addClient model infrastructure =
  if demand model > numberOfClients infrastructure then
    addOrFillRackWithClient model infrastructure <| numberOfClientsToAdd model
  else
    infrastructure

addOrFillRackWithClient model infrastructure clientsToAdd =
  case (List.head <| List.filter hasRoomForClients (racks infrastructure)) of
    Just infra ->
      case infra of
        Rack info ->
          let
            freeSlots = maxClients infra - info.client
            overfilledSlots = clientsToAdd - freeSlots
            slotsToAdd = Basics.min freeSlots clientsToAdd
            newInfrastructure = updateInfrastructure infrastructure <| Rack { info | client = info.client + slotsToAdd }
          in
            if overfilledSlots > 0 then
              addOrFillRackWithClient model newInfrastructure overfilledSlots
            else
              newInfrastructure
        Workplace _ ->
          infrastructure
        Doorway _ ->
          infrastructure
    Nothing ->
      infrastructure

numberOfMaxClients infrastructure =
  List.sum <| List.map maxClients <| racks infrastructure

maxClients infrastructure =
  case infrastructure of
    Rack info ->
      case info.system of
        Just system ->
          case system of
            Linux ->
              42
            Windows ->
              42
            Xen ->
              840
        Nothing ->
          0
    Workplace _ ->
      0
    Doorway _ ->
      0

numberOfClients infrastructure =
  List.sum <| List.map countClients infrastructure

countClients infrastructure =
  case infrastructure of
    Rack info ->
      info.client
    Workplace _ ->
      0
    Doorway _ ->
      0

hasRoomForClients infrastructure =
  case infrastructure of
    Rack info ->
      info.client < maxClients infrastructure
    Workplace _ ->
      False
    Doorway _ ->
      False

hasClient query infrastructure =
  case infrastructure of
    Rack info ->
      if query then
        info.client > 0
      else
        info.client <= 0
    Workplace _ ->
      False
    Doorway _ ->
      False

canBuildAt infrastructure position =
  case infrastructureAt infrastructure position of
    Just _ ->
      False
    Nothing ->
      let
        newInfrastructure = buildRack infrastructure position
      in
        if allHaveFreeNeighbors newInfrastructure then
          True
        else
          False

demand model =
  -- TODO: replace by a smart formula
  numberOfMaxClients model.infrastructure

numberOfClientsToAdd model =
  ceiling <| 0.2 * (toFloat <| (demand model - numberOfClients model.infrastructure))

countFreeNeighbors infrastructure position =
  let
    neighborPositions = [ topOf position
                        , leftOf position
                        , rightOf position
                        , bottomOf position
                        ]
    neighbors = List.filterMap (toInfrastructure infrastructure) neighborPositions
    neighborCount = List.length <| List.filterMap identity neighbors
  in
    List.length neighbors - neighborCount

hasFreeNeighbors infrastructure position =
  countFreeNeighbors infrastructure position >= 1

toInfrastructure infrastructure position =
  case position of
    Just position ->
      Just <| infrastructureAt infrastructure position
    Nothing ->
      Nothing

allHaveFreeNeighbors infrastructure =
  List.all ((==) True)
    <| List.map (hasFreeNeighbors infrastructure)
    <| List.map positionOf infrastructure
