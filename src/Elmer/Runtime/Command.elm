module Elmer.Runtime.Command exposing
  ( commandRunner
  , mapState
  , generate
  , fail
  , stub
  , defer
  )


import Elmer.Runtime.Intention as Intention
import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Command.Task as Promise
import Elmer.Runtime.Command.MapState as MapState
import Elmer.Runtime.Command.Generate as Generate
import Elmer.Runtime.Command.Fail as Fail
import Elmer.Runtime.Command.Stub as Stub
import Elmer.Runtime.Command.Defer as Defer
import Elmer.Context as Context exposing (Context)
import Elmer.Printer exposing (..)
import Dict exposing (Dict)


commandRunners : Dict String (CommandRunner model subMsg msg)
commandRunners =
  Dict.fromList
    [ ( Fail.name, Fail.commandRunner )
    , ( Stub.name, Stub.commandRunner )
    , ( Generate.name, Generate.commandRunner )
    , ( MapState.name, MapState.commandRunner )
    , ( Promise.name, Promise.commandRunner )
    ]


commandRunner : String -> CommandRunner model subMsg msg
commandRunner name =
  Dict.get name commandRunners
    |> Maybe.withDefault (unknownCommandRunner name)


mapState : typeId -> (Maybe a -> a) -> Cmd msg
mapState =
  MapState.with


generate : (Context model msg -> Cmd msg) -> Cmd msg
generate =
  Generate.with


fail : String -> Cmd msg
fail =
  Fail.with


stub : msg -> Cmd msg
stub =
  Stub.with


defer : Cmd msg -> Cmd msg
defer =
  Defer.with
  

unknownCommandRunner : String -> CommandRunner model subMsg msg
unknownCommandRunner commandName _ _ =
  CommandError <|
    format <|
      [ message "Elmer encountered a command it does not know how to run" commandName
      , description "Try sending a stubbed or dummy command instead"
      ]
