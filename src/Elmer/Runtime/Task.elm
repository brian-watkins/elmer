module Elmer.Runtime.Task exposing
  ( mapState
  , abortWith
  , abandon
  , defer
  )

import Json.Encode as Encode exposing (Value)
import Elmer.Value as Value
import Task exposing (Task)
import Elmer.Runtime.Command.MapState


defer : Task x a -> Task x a
defer task =
  Value.encode (Value.for 1003)
    [ ( "task", Value.for task )
    ]


mapState : typeId -> (Maybe a -> a) -> Task x b -> Task x b
mapState typeId mapper =
  andDo <|
    Elmer.Runtime.Command.MapState.with typeId mapper


andDo : Cmd msg -> Task x a -> Task x a
andDo command task =
  Value.encode (Value.for 1001)
    [ ( "task", Value.for task )
    , ( "command", Value.for command )
    ]


abortWith : Cmd msg -> Task x a
abortWith command =
  Value.encode (Value.for 1002)
    [ ( "command", Value.for command )
    ]


abandon : Task x a
abandon =
  abortWith Cmd.none
