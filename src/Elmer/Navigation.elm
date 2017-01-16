module Elmer.Navigation
    exposing
        ( setLocation
        , expectLocation
        , fakeNavigateCommand
        )

import Elmer.Command as Command
import Elmer.Types exposing (..)
import Elmer
import Expect
import Elmer.Printer exposing (..)


fakeNavigateCommand : String -> Cmd msg
fakeNavigateCommand url =
  Native.Helpers.toCmd "Elmer_Navigation" url

expectLocation : String -> ComponentStateResult model msg -> Expect.Expectation
expectLocation expectedURL =
  Elmer.mapToExpectation <|
      \componentState ->
          case componentState.location of
              Just location ->
                  Expect.equal location expectedURL
                      |> Expect.onFail (format [message "Expected to be at location:" expectedURL, message "but location is:" location])

              Nothing ->
                  Expect.fail (format [message "Expected to be at location:" expectedURL, description "but no location has been set"])


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
                          Command.send command componentStateResult
                    Nothing ->
                        UpstreamFailure "setLocation failed because no locationParser was set"
            )
