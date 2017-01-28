module Elmer.Command.Internal exposing
  ( mapState
  )

import Elmer.Types exposing (..)

mapState : (HtmlComponentState model msg -> HtmlComponentState model msg) -> Cmd msg
mapState mapper =
  Native.Helpers.toCmd "Elmer_MapState" mapper
