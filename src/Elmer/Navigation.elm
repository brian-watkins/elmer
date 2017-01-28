module Elmer.Navigation
    exposing
        ( setLocation
        , expectLocation
        , fakeNavigateCommand
        )

import Elmer.Command as Command
import Elmer.Command.Internal as InternalCommand
import Elmer.Types exposing (..)
import Elmer
import Expect
import Elmer.Printer exposing (..)
import Elmer.Navigation.Location as Location
import Navigation


fakeNavigateCommand : String -> Cmd msg
fakeNavigateCommand url =
  let
    parseCommand = InternalCommand.generate <| generateCommandForLocation url
    stateCommand = InternalCommand.mapState <| storeLocation url
  in
    Cmd.batch [ stateCommand, parseCommand ]

generateCommandForLocation : String -> HtmlComponentState model msg -> Cmd msg
generateCommandForLocation url componentState =
  case componentState.locationParser of
    Just locationParser ->
      let
        message = handleLocationUpdate url locationParser
      in
        Command.stubbedCommand message
    Nothing ->
      Cmd.none

handleLocationUpdate : String -> (Navigation.Location -> msg) -> msg
handleLocationUpdate url parser =
    (parser (Location.asLocation url))

storeLocation : String -> HtmlComponentState model msg -> HtmlComponentState model msg
storeLocation url componentState =
  { componentState | location = Just url }


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
