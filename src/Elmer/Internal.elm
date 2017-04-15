module Elmer.Internal exposing
  ( boolToString
  )


boolToString : Bool -> String
boolToString bool =
  Basics.toString bool
    |> String.toLower
