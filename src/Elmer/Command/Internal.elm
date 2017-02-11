module Elmer.Command.Internal exposing
  ( mapState
  , generate
  )

import Elmer.Types exposing (..)
import Elmer.Platform as Platform

mapState : (HtmlComponentState model msg -> HtmlComponentState model msg) -> Cmd msg
mapState mapper =
  Platform.toCmd "Elmer_MapState" mapper

generate : (HtmlComponentState model msg -> Cmd msg) -> Cmd msg
generate generator =
  Platform.toCmd "Elmer_Generate" generator
