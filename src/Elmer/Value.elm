module Elmer.Value exposing
  ( for
  , decode
  , encode
  , constructor
  , field
  , mapArg
  , mapArg2
  , mapArg3
  , global
  , cast
  , nativeType
  , assign
  , decoder
  )

import Json.Decode as Json
import Json.Encode as Encode
import Elm.Kernel.Value

type Value
  = Value

for : a -> Value
for =
  cast

cast : a -> b
cast =
  Elm.Kernel.Value.cast


nativeType : a -> String
nativeType =
  Elm.Kernel.Value.nativeType


assign : String -> v -> v
assign =
  Elm.Kernel.Value.assign


decode : Json.Decoder a -> v -> Result String b
decode valueDecoder value =
  Elm.Kernel.Value.wrap value
    |> cast
    |> Json.decodeValue valueDecoder
    |> Result.mapError cast
    |> Result.map cast
    |> Result.map (\v -> Elm.Kernel.Value.unwrap v |> cast)


decoder : Json.Decoder a
decoder =
  Json.map Elm.Kernel.Value.unwrap Json.value


constructor : v -> String
constructor =
  field "$"


field : String -> v -> b
field key value =
  case decode (Json.field key Json.value) value of
    Ok f ->
      f
    Err msg ->
      "Error decoding field " ++ key ++ ":\n\n" ++ msg ++ "\n"
        |> Debug.todo


global : String -> a
global =
  Elm.Kernel.Value.global


mapArg : (a -> z) -> v -> z
mapArg =
  mapArgAt "a"


mapArg2 : (a -> b -> z) -> v -> z
mapArg2 mapper value =
  mapArg mapper value
    |> flip (mapArgAt "b") value


mapArg3 : (a -> b -> c -> z) -> v -> z
mapArg3 mapper value =
  mapArg2 mapper value
    |> flip (mapArgAt "c") value


mapArgAt : String -> (a -> z) -> v -> z
mapArgAt index mapper value =
  argAt index value
    |> mapper


argAt : String -> v -> a
argAt index value =
  field index value


encode : Value -> List (String, Value) -> b
encode ctor args =
  ( "$", Elm.Kernel.Value.wrap ctor ) :: (List.map (\(key, value) -> (key, Elm.Kernel.Value.wrap value)) args)
    |> Encode.object
    |> Elm.Kernel.Value.unwrap
    |> cast
  

flip : (a -> b -> c) -> b -> a -> c
flip fun bArg aArg =
  fun aArg bArg