module Elmer.Spy.Function exposing
  ( Function
  , Argument
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
import Json.Encode as Encode
import Elm.Kernel.Function
import Elmer.Value as Value


type alias Function =
  { alias: String
  , identifier: String
  , original: Value
  , fake: Value
  }

type alias Argument =
  Encode.Value

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


activateSpy : List (List Argument) -> Function -> Function
activateSpy calls function =
  let
    callValues =
      Encode.list (Encode.list identity) calls
        |> Elm.Kernel.Value.unwrap
  in
  Value.assign function.identifier function.fake
    |> Elm.Kernel.Function.activate function.alias callValues
    |> always function


deactivateSpy : Function -> List (List Argument)
deactivateSpy function =
  Value.assign function.identifier function.original
    |> always (Elm.Kernel.Function.deactivate function.alias)
    |> Value.decode (Json.list <| Json.list argumentDecoder)
    |> Result.withDefault []


argumentDecoder : Json.Decoder Argument
argumentDecoder =
  Json.value


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
