module Elmer.Spy.Function exposing
  ( Function
  , from
  , create
  , withFake
  , callable
  , activateSpy
  , deactivateSpy
  )

import Json.Decode as Json exposing (Value)
import Native.Function
import Elmer.Value as Value


type alias Function =
  { alias: String
  , identifier: Maybe String
  , original: Maybe Value
  , fake: Value
  }


from : String -> (() -> a) -> Maybe Function
from functionAlias namingThunk =
  Native.Function.globalIdentifier namingThunk
    |> Value.decode identifierDecoder
    |> Result.withDefault Nothing
    |> Maybe.map (\globalId ->
        { alias = functionAlias
        , identifier = Just globalId
        , original = Just <| Value.global globalId
        , fake =
            Value.global globalId
              |> recordable functionAlias
        }
    )


recordable : String -> (a -> b) -> Value
recordable =
  Native.Function.recordable


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
        |> Native.Function.activate function.alias
        |> always function
    Nothing ->
      Native.Function.activate function.alias function.fake
        |> always function


deactivateSpy : Function -> Json.Value
deactivateSpy function =
  case Maybe.map2 (,) function.identifier function.original of
    Just (identifier, original) ->
      Value.assign identifier original
        |> always (Native.Function.deactivate function.alias)
    Nothing ->
      Native.Function.deactivate function.alias


callable : String -> (a -> b)
callable name =
  \args ->
    let
      spyFunc =
        Native.Function.active name
          |> Value.decode (Json.nullable Json.value)
          |> Result.withDefault Nothing
    in
      case spyFunc of
        Just spy ->
          spy args
        Nothing ->
          "Attempted to use Spy.callable with an unknown spy: " ++ name
            |> Debug.crash
