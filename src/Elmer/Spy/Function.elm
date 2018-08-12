module Elmer.Spy.Function exposing
  ( Function
  , globalIdentifier
  , from
  , create
  , replace
  , withFake
  , callable
  , activateSpy
  , deactivateSpy
  )

import Json.Decode as Json exposing (Value)
import Elm.Kernel.Function
import Elmer.Value as Value


type alias Function =
  { alias: String
  , identifier: Maybe String
  , original: Maybe Value
  , fake: Value
  }

globalIdentifier : (() -> a) -> Maybe String
globalIdentifier namingThunk =
  Elm.Kernel.Function.globalIdentifier namingThunk
    |> Value.decode identifierDecoder
    |> Result.withDefault Nothing


from : String -> (() -> a) -> Maybe Function
from functionAlias namingThunk =
  globalIdentifier namingThunk
    |> Maybe.map (\globalId ->
        { alias = functionAlias
        , identifier = Just globalId
        , original = Just <| Value.global globalId
        , fake =
            Value.global globalId
              |> recordable functionAlias
        }
    )


replace : (() -> a) -> b -> Maybe Function
replace namingThunk value =
  globalIdentifier namingThunk
    |> Maybe.andThen (\globalId ->
        if isValue globalId then
          Just
            { alias = globalId
            , identifier = Just globalId
            , original = Just <| Value.global globalId
            , fake = Value.cast value
            }
        else
          Nothing
    )


isValue : String -> Bool
isValue globalId =
  Value.global globalId
    |> Value.nativeType
    |> (/=) "function"


recordable : String -> (a -> b) -> Value
recordable =
  Elm.Kernel.Function.recordable


identifierDecoder : Json.Decoder (Maybe String)
identifierDecoder =
  Json.nullable Json.string


create : String -> (a -> b) -> Function
create name impl =
  { alias = name
  , identifier = Nothing
  , original = Nothing
  , fake =
      Value.cast impl
        |> recordable name
  }


withFake : (a -> b) -> Function -> Function
withFake fake function =
  { function
  | fake =
      Value.cast fake
        |> recordable function.alias
  }


activateSpy : Function -> Function
activateSpy function =
  case function.identifier of
    Just identifier ->
      Value.assign identifier function.fake
        |> Elm.Kernel.Function.activate function.alias
        |> always function
    Nothing ->
      Elm.Kernel.Function.activate function.alias function.fake
        |> always function


deactivateSpy : Function -> Json.Value
deactivateSpy function =
  case Maybe.map2 toTuple function.identifier function.original of
    Just (identifier, original) ->
      Value.assign identifier original
        |> always (Elm.Kernel.Function.deactivate function.alias)
    Nothing ->
      Elm.Kernel.Function.deactivate function.alias

toTuple : a -> b -> (a, b)
toTuple aArg bArg =
  ( aArg, bArg )

callable : String -> (a -> b)
callable name =
  \args ->
    let
      spyFunc =
        Elm.Kernel.Function.active name
          |> Value.decode (Json.nullable Value.decoder)
          |> Result.withDefault Nothing
    in
      case spyFunc of
        Just spy ->
          spy args
        Nothing ->
          "Attempted to use Spy.callable with an unknown spy: " ++ name
            |> Debug.todo
