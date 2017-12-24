module Elmer.Value exposing
  ( decode
  , constructor
  , field
  , mapArg
  , mapArg2
  )

import Json.Decode as Json
import Native.Value

decode : Json.Decoder a -> v -> Result String b
decode decoder value =
  Native.Value.decode decoder value


constructor : v -> String
constructor value =
  decode (Json.field "ctor" Json.string) value
    |> Result.withDefault ""


field : String -> v -> b
field key value =
  case decode (Json.field key Json.value) value of
    Ok f ->
      f
    Err msg ->
      "Error decoding field " ++ key ++ ":\n\n" ++ msg ++ "\n"
        |> Debug.crash


mapArg : (a -> z) -> v -> z
mapArg mapper value =
  argAt 0 value
    |> mapper


mapArg2 : (a -> b -> z) -> v -> z
mapArg2 mapper value =
  let
    firstArg = argAt 0 value
    secondArg = argAt 1 value
  in
    mapper firstArg secondArg


argAt : Int -> v -> a
argAt index value =
  field ("_" ++ toString index) value
