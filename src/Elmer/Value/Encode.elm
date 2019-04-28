module Elmer.Value.Encode exposing
  ( for
  , encode
  )

import Json.Encode as Encode
import Elmer.Value.Native as Native

type Value
  = Value

for : a -> Value
for =
  Native.cast

encode : Value -> List (String, Value) -> b
encode ctor args =
  ( "$", Native.wrap ctor ) :: (List.map (\(key, value) -> (key, Native.wrap value)) args)
    |> Encode.object
    |> Native.unwrap
    |> Native.cast
