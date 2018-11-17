module Elmer.Spy.Function exposing
  ( Function
  , globalIdentifier
  , functionIdentifier
  , from
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
  , identifier: String
  , original: Value
  , fake: Value
  }

globalIdentifier : (() -> a) -> Maybe String
globalIdentifier namingThunk =
  Elm.Kernel.Function.globalIdentifier namingThunk
    |> Value.decode identifierDecoder
    |> Result.withDefault Nothing


readableIdentifier : String -> String
readableIdentifier globalId =
  String.split "$" globalId
    |> List.drop 2
    |> String.join "."


functionIdentifier : (() -> a) -> Maybe String
functionIdentifier identifier =
  globalIdentifier identifier
    |> Maybe.map readableIdentifier


from : (() -> a) -> Maybe Function
from namingThunk =
  globalIdentifier namingThunk
    |> Maybe.map (\globalId ->
        let
          functionAlias =
            readableIdentifier globalId
        in
          { alias = functionAlias
          , identifier = globalId
          , original = Value.global globalId
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
            , identifier = globalId
            , original = Value.global globalId
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


withFake : (a -> b) -> Function -> Function
withFake fake function =
  { function
  | fake =
      Value.cast fake
        |> recordable function.alias
  }


activateSpy : Function -> Function
activateSpy function =
  Value.assign function.identifier function.fake
    |> Elm.Kernel.Function.activate function.alias
    |> always function


deactivateSpy : Function -> Json.Value
deactivateSpy function =
  Value.assign function.identifier function.original
    |> always (Elm.Kernel.Function.deactivate function.alias)


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
