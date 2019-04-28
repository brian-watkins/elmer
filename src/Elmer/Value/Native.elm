module Elmer.Value.Native exposing
  ( global
  , cast
  , nativeType
  , assign
  , wrap
  , unwrap
  , decode
  , decoder
  , field
  , constructor
  )

import Json.Decode as Json
import Elm.Kernel.Value


constructor : Json.Decoder Int
constructor =
  Json.field "$" Json.int


field : String -> Json.Decoder a
field key =
  Json.field key decoder


decoder : Json.Decoder a
decoder =
  Json.map unwrap Json.value


decode : Json.Decoder a -> v -> Result Json.Error b
decode valueDecoder value =
  wrap value
    |> cast
    |> Json.decodeValue valueDecoder
    |> Result.mapError cast
    |> Result.map cast
    |> Result.map (\v -> unwrap v |> cast)


cast : a -> b
cast =
  Elm.Kernel.Value.cast


nativeType : a -> String
nativeType =
  Elm.Kernel.Value.nativeType


assign : String -> v -> v
assign =
  Elm.Kernel.Value.assign


global : String -> a
global =
  Elm.Kernel.Value.global


wrap : a -> v
wrap =
  Elm.Kernel.Value.wrap


unwrap : a -> v
unwrap =
  Elm.Kernel.Value.unwrap