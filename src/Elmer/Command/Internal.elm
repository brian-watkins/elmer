module Elmer.Command.Internal exposing
  ( mapState
  , generate
  )

import Elmer.Internal exposing (..)
import Elmer.Platform as Platform

mapState : (Component model msg -> Component model msg) -> Cmd msg
mapState mapper =
  Platform.toCmd "Elmer_MapState" mapper

generate : (Component model msg -> Cmd msg) -> Cmd msg
generate generator =
  Platform.toCmd "Elmer_Generate" generator
