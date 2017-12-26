module Elmer.Value exposing
  ( decode
  , constructor
  , field
  , mapArg
  , mapArg2
  , mapArg3
  , global
  )

import Json.Decode as Json
import Native.Value


cast : a -> b
cast =
  Native.Value.cast

decode : Json.Decoder a -> v -> Result String b
decode decoder value =
  cast value
    |> Json.decodeValue decoder
    |> Result.map cast


constructor : v -> String
constructor =
  field "ctor"


field : String -> v -> b
field key value =
  case decode (Json.field key Json.value) value of
    Ok f ->
      f
    Err msg ->
      "Error decoding field " ++ key ++ ":\n\n" ++ msg ++ "\n"
        |> Debug.crash


global : String -> a
global =
  Native.Value.global


mapArg : (a -> z) -> v -> z
mapArg =
  mapArgAt 0


mapArg2 : (a -> b -> z) -> v -> z
mapArg2 mapper value =
  mapArg mapper value
    |> flip (mapArgAt 1) value


mapArg3 : (a -> b -> c -> z) -> v -> z
mapArg3 mapper value =
  mapArg2 mapper value
    |> flip (mapArgAt 2) value


mapArgAt : Int -> (a -> z) -> v -> z
mapArgAt index mapper value =
  argAt index value
    |> mapper


argAt : Int -> v -> a
argAt index value =
  field ("_" ++ toString index) value
