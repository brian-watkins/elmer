module Elmer.Navigation.Runner
    exposing
        ( handleLocationUpdate
        )

import Elmer exposing (..)
import Elmer.Navigation.Location as Location
import Navigation


handleLocationUpdate : String -> (Navigation.Location -> msg) -> msg
handleLocationUpdate url parser =
    (parser (Location.asLocation url))
