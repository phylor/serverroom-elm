module Dialog exposing (..)

type alias Dialog messageType =
  { message : String
  , options : List (DialogOption messageType)
  }

type alias DialogOption messageType =
  { title : String
  , action : messageType
  }

