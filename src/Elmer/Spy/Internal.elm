module Elmer.Spy.Internal exposing
  ( Spy(..)
  , Calls
  , create
  , replaceValue
  , callable
  , activate
  , deactivate
  , calls
  , spiesFrom
  , withSpies
  , withSpiesFor
  , registerFake
  )

import Elmer.Context as Context exposing (Context)
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Spy.Function as Function exposing (Function)
import Elmer.Spy.Arg as Arg exposing (Arg)
import Json.Decode as Json
import Elmer.Value as Value
import Expect


type alias Calls =
  { name : String
  , calls : List (List Arg)
  }


type Spy
  = Uninstalled (() -> Spy)
  | Active SpyValue
  | Inactive SpyValue
  | Error SpyError
  | Batch (List Spy)


type alias SpyValue =
  { name: String
  , function: Function
  , calls: List (List Function.Argument)
  }

type alias SpyError =
  { reason: String
  }

create : (() -> a) -> Spy
create namingFunc =
  case Function.from namingFunc of
    Just function ->
      recordCalls
        { name = function.alias
        , function = function
        , calls = []
        }
    Nothing ->
      Error
        { reason = "Unable to identify a function to spy on"
        }


replaceValue : (() -> a) -> b -> Spy
replaceValue namingFunc value =
  case Function.replace namingFunc value of
    Just function ->
      recordCalls
        { name = function.alias
        , function = function
        , calls = []
        }
    Nothing ->
      let
        valueName =
          Function.functionIdentifier namingFunc
      in
        case valueName of
          Just name ->
            Error
              { reason = name ++ " is a function, but your test is treating it as a value to be replaced"
              }
          Nothing ->
            Error
              { reason = "Unable to identify a value to replace"
              }


recordCalls : SpyValue -> Spy
recordCalls spy =
  Active
    { spy | function = Function.activateSpy spy.calls spy.function }


callable : String -> (a -> b)
callable name =
  Function.callable name


calls : String -> List Spy -> Maybe Calls
calls name spies =
  List.filterMap (\spy ->
    case spy of
      Active spyValue ->
        callsIfName name spyValue
      Inactive spyValue ->
        callsIfName name spyValue
      _ ->
        Nothing
  ) spies
    |> List.head


callsIfName : String -> SpyValue -> Maybe Calls
callsIfName name spyValue =
  if spyValue.name == name then
    Just <| callRecord spyValue
  else
    Nothing


callRecord : SpyValue -> Calls
callRecord spyValue =
  { name = spyValue.name
  , calls = decodeArguments spyValue.calls
  }


decodeArguments : List (List Function.Argument) -> List (List Arg)
decodeArguments =
  List.map <| List.map <|
    \arg ->
      Json.decodeValue Arg.decoder arg
        |> Result.withDefault Arg.AnyArg


registerFake : (a -> b) -> SpyValue -> Spy
registerFake fake spy =
  Active
    { spy | function = Function.withFake fake spy.function
    }


activate : List Spy -> List Spy
activate spies =
  List.map (\spy ->
    case spy of
      Uninstalled installer ->
        [ installer () ]
      Inactive spyValue ->
        [ recordCalls spyValue ]
      Batch batched ->
        activate batched
      _ ->
        [ spy ]
  ) spies
    |> List.concat


deactivateOne : Spy -> Spy
deactivateOne spy =
  case spy of
    Active spyValue ->
      Inactive
        { spyValue | calls = Function.deactivateSpy spyValue.function }
    _ ->
      spy


deactivate : List Spy -> List Spy
deactivate =
  List.map deactivateOne


type SpyState
  = Spies


spiesFrom : Context model msg -> List Spy
spiesFrom context =
  Context.state Spies context
    |> Maybe.withDefault []


withSpies : List Spy -> Context model msg -> Context model msg
withSpies spies context =
  RuntimeCommand.mapState Spies (\_ -> spies)
    |> Context.updateStateFor context

withSpiesFor : Context model msg -> List Spy -> Context model msg
withSpiesFor context spies =
  withSpies spies context