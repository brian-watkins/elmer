module Elmer.Command.Internal exposing
  ( mapState
  , generate
  )

import Elmer.Types exposing (..)

mapState : (HtmlComponentState model msg -> HtmlComponentState model msg) -> Cmd msg
mapState mapper =
  Native.Helpers.toCmd "Elmer_MapState" mapper

generate : (HtmlComponentState model msg -> Cmd msg) -> Cmd msg
generate generator =
  Native.Helpers.toCmd "Elmer_Generate" generator
