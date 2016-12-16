module Elmer.Navigation
    exposing
        ( setLocation
        , expectLocation
        , fakeNavigateCommand
        )

import Elmer.Event as Event
import Elmer.Types exposing (..)
import Elmer
import Expect
import Json.Encode as Encode


fakeNavigateCommand : String -> Cmd msg
fakeNavigateCommand url =
  Native.Helpers.toCmd "Elmer_Navigation" (navigationCommandData url)

navigationCommandData : String -> Encode.Value
navigationCommandData url =
  Encode.object
    [ ("url", Encode.string url )
    ]

expectLocation : String -> ComponentStateResult model msg -> Expect.Expectation
expectLocation expectedURL =
  Elmer.mapToExpectation <|
      \componentState ->
          case componentState.location of
              Just location ->
                  Expect.equal location expectedURL
                      |> Expect.onFail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut location is:\n\n\t" ++ location)

              Nothing ->
                  Expect.fail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut no location has been set")


setLocation : String -> ComponentStateResult model msg -> ComponentStateResult model msg
setLocation location componentStateResult =
    componentStateResult
        |> Elmer.map
            (\componentState ->
                case componentState.locationParser of
                    Just locationParser ->
                      let
                          command = fakeNavigateCommand location
                      in
                          Event.sendCommand command componentStateResult
                    Nothing ->
                        UpstreamFailure "setLocation failed because no locationParser was set"
            )
