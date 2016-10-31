module Elmer.Browser exposing (expectLocation)

import Expect
import Elmer exposing (..)

type alias BrowserState =
  { location: Maybe String
  }

browserState : BrowserState
browserState =
  Native.Helpers.browserState

expectLocation : String -> ComponentStateResult model msg -> Expect.Expectation
expectLocation expectedURL componentStateResult =
  case componentStateResult of
    CurrentState _ ->
      case browserState.location of
        Just location ->
          Expect.equal location expectedURL
            |> Expect.onFail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut location is:\n\n\t" ++ location)
        Nothing ->
          Expect.fail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut no location has been set")
    UpstreamFailure msg ->
      Expect.fail msg
