module Elmer.UrlHelpers exposing (..)

import Url exposing (Url)


asUrl : String -> Url
asUrl urlString =
  case Url.fromString urlString of
    Just url ->
      url
    Nothing ->
      Debug.todo <| "Could not parse url: " ++ urlString