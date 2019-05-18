module Elmer.Spy.Function exposing
  ( Function
  , Argument
  , globalIdentifier
  , functionIdentifier
  , from
  , replace
  , withFake
  , activateSpy
  , deactivateSpy
  )

import Json.Decode as Json exposing (Value)
import Json.Encode as Encode
import Elm.Kernel.Function
import Elmer.Value.Native as Native
import Elmer.Errors as Errors


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
    |> Native.decode identifierDecoder
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


from : (() -> a) -> Result String Function
from namingThunk =
  globalIdentifier namingThunk
    |> Result.fromMaybe (Errors.print <| Errors.unableToIdentifySpy)
    |> Result.andThen (\globalId ->
      let
        functionAlias =
          readableIdentifier globalId
      in
        if Elm.Kernel.Function.isActive functionAlias then
          Errors.spyAlreadyObserved functionAlias
            |> Errors.print
            |> Err
        else
          Ok 
            { alias = functionAlias
            , identifier = globalId
            , original = Native.global globalId
            , fake =
                Native.global globalId
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
            , original = Native.global globalId
            , fake = Native.cast value
            }
        else
          Nothing
    )


isValue : String -> Bool
isValue globalId =
  Native.global globalId
    |> Native.nativeType
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
      Native.cast fake
        |> recordable function.alias
  }


activateSpy : List (List Argument) -> Function -> Function
activateSpy calls function =
  let
    callValues =
      Encode.list (Encode.list identity) calls
        |> Native.unwrap
  in
  Native.assign function.identifier function.fake
    |> Elm.Kernel.Function.activate function.alias callValues
    |> always function


deactivateSpy : Function -> List (List Argument)
deactivateSpy function =
  Native.assign function.identifier function.original
    |> always (Elm.Kernel.Function.deactivate function.alias)
    |> Native.decode (Json.list <| Json.list argumentDecoder)
    |> Result.withDefault []


argumentDecoder : Json.Decoder Argument
argumentDecoder =
  Json.value
