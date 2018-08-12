module Elmer.Internal exposing
  ( boolToString
  )


boolToString : Bool -> String
boolToString bool =
  case bool of
    True ->
      "true"
    False ->
      "false"
