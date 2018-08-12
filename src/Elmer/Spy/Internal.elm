module Elmer.Spy.Internal exposing
  ( Spy(..)
  , Calls
  , create
  , createWith
  , replaceValue
  , callable
  , activate
  , deactivate
  , calls
  , batch
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
  | Error SpyValue
  | Batch (List Spy)


type alias SpyValue =
  { name: String
  , function: Maybe Function
  , calls: List (List Arg)
  }


create : String -> (() -> a) -> Spy
create name namingFunc =
  case Function.from name namingFunc of
    Just function ->
      recordCalls
        { name = name
        , function = Just function
        , calls = []
        }
    Nothing ->
      Error
        { name = name
        , function = Nothing
        , calls = []
        }


createWith : String -> (a -> b) -> Spy
createWith name fakeFunction =
  recordCalls
    { name = name
    , function = Just <| Function.create name fakeFunction
    , calls = []
    }


replaceValue : (() -> a) -> b -> Spy
replaceValue namingFunc value =
  let
    globalId =
      Function.globalIdentifier namingFunc
        |> Maybe.withDefault "Unable to find function to replace"
    functionName =
      String.split "$" globalId
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "Unable to find function to replace"
  in
    case Function.replace namingFunc value of
      Just function ->
        recordCalls
          { name = globalId
          , function = Just function
          , calls = []
          }
      Nothing ->
        Error
          { name = functionName
          , function = Nothing
          , calls = []
          }


recordCalls : SpyValue -> Spy
recordCalls spy =
  case spy.function of
    Just function ->
      Active
        { spy | function = Just <| Function.activateSpy function }
    Nothing ->
      Error spy


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
  , calls = spyValue.calls
  }


registerFake : (a -> b) -> SpyValue -> Spy
registerFake fake spy =
  case spy.function of
    Just function ->
      Active
        { spy | function = Just <| Function.withFake fake function
        }
    Nothing ->
      Active spy


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
      let
        recordedCalls =
          case spyValue.function of
            Just function ->
              Function.deactivateSpy function
                |> Value.decode (Json.list <| Json.list Arg.decoder)
                |> Result.withDefault []
            Nothing ->
              []
      in
        Inactive
          { spyValue | calls = List.append spyValue.calls recordedCalls }
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