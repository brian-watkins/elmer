module Elmer.Navigation.Runner exposing
  ( updateLocation
  )

import Elmer exposing (..)
import Elmer.Navigation.Location as Location

import Navigation
import Json.Decode as Json exposing ((:=))


updateLocation : String -> (Navigation.Parser navData) -> (navData -> model -> (model, Cmd msg)) -> model -> (model, Cmd msg)
updateLocation url parser urlUpdate model =
  let
    locationParser = Native.Helpers.asLocationParser parser
  in
    urlUpdate (locationParser (Location.asLocation url)) model
