module Elmer.Shared exposing (..)

import Elmer.Types exposing(..)

componentStateOrFail : ComponentStateResult model msg -> (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg
componentStateOrFail componentStateResult stateFunction =
  case componentStateResult of
    CurrentState componentState ->
      stateFunction componentState
    UpstreamFailure message ->
      UpstreamFailure message
