module Elmer.Runtime.Promise.Types exposing
  ( Promise(..)
  , Resolution(..)
  , Continuation
  , Promised
  )

import Json.Decode as Json exposing (Value)


type Promise msg
  = Complete (Resolution msg)
  | Continue (Continuation msg)
  | AndDo (Cmd msg) (Promise msg)
  | Defer (Promise msg)


type alias Continuation msg =
  { next : Promise msg
  , onResolve : Maybe (Value -> Value)
  , onReject : Maybe (Value -> Value)
  }


type Resolution msg
  = Resolved Value
  | Rejected Value
  | Aborted (Cmd msg)


type alias Promised msg =
  { resolution: Resolution msg
  , shouldDefer: Bool
  , commands: List (Cmd msg)
  }
