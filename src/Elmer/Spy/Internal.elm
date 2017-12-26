module Elmer.Spy.Internal exposing
  ( Spy(..)
  , Calls
  , Arg(..)
  , create
  , createWith
  , callable
  , activate
  , deactivate
  , calls
  , batch
  , spiesFrom
  , withSpies
  , registerFake
  )

import Elmer.Context as Context exposing (Context)
import Elmer.Runtime.Command as RuntimeCommand
import Native.Spy

type alias Calls =
  { name : String
  , calls : List (List Arg)
  }

type Arg
  = StringArg String
  | IntArg Int
  | FloatArg Float
  | BoolArg Bool
  | TypedArg String
  | FunctionArg
  | AnyArg

type Spy
  = Uninstalled (() -> Spy)
  | Active SpyValue
  | Inactive SpyValue
  | Error SpyValue
  | Batch (List Spy)

type SpyValue
  = SpyValue


create : String -> (() -> a) -> Spy
create name namingFunc =
  Native.Spy.create name namingFunc

createWith : String -> (a -> b) -> Spy
createWith name fakeFunction =
  Native.Spy.createWith name fakeFunction

callable : String -> (a -> b)
callable name =
  Native.Spy.callable name

calls : String -> List Spy -> Maybe Calls
calls name spies =
  List.filterMap (\spy ->
    case spy of
      Active spyValue ->
        callsWithName name spyValue
      Inactive spyValue ->
        callsWithName name spyValue
      _ ->
        Nothing
  ) spies
    |> List.head

callsWithName : String -> SpyValue -> Maybe Calls
callsWithName name spyValue =
  let
    calls = Native.Spy.calls spyValue
  in
    if calls.name == name then
      Just calls
    else
      Nothing

registerFake : (a -> b) -> SpyValue -> Spy
registerFake =
  Native.Spy.registerFake

{-| Note: Calling a fake method on a batch spy is not supported
-}
batch : List Spy -> Spy
batch spies =
  Batch spies

activate : List Spy -> List Spy
activate spies =
  List.map (\spy ->
    case spy of
      Uninstalled installer ->
        installer () :: []
      Inactive spyValue ->
        Native.Spy.activate spyValue :: []
      Batch spies ->
        activate spies
      _ ->
        spy :: []
  ) spies
    |> List.concat

deactivateOne : Spy -> Spy
deactivateOne spy =
  case spy of
    Active spyValue ->
      Native.Spy.deactivate spyValue
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
    |> flip Context.updateState context
