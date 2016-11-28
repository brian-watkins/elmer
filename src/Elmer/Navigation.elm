module Elmer.Navigation
    exposing
        ( setLocation
        , expectLocation
        )

import Navigation
import Elmer.Event as Event
import Elmer.Types exposing (..)
import Elmer
import Expect


expectLocation : String -> ComponentStateResult model msg -> Expect.Expectation
expectLocation expectedURL componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            case componentState.location of
                Just location ->
                    Expect.equal location expectedURL
                        |> Expect.onFail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut location is:\n\n\t" ++ location)

                Nothing ->
                    Expect.fail ("Expected to be at location:\n\n\t" ++ expectedURL ++ "\n\nbut no location has been set")

        UpstreamFailure msg ->
            Expect.fail msg


setLocation : String -> ComponentStateResult model msg -> ComponentStateResult model msg
setLocation location componentStateResult =
    componentStateResult
        |> Elmer.map
            (\componentState ->
                case componentState.locationParser of
                    Just locationParser ->
                      let
                          command = Navigation.newUrl location
                      in
                          Event.sendCommand command componentStateResult
                    Nothing ->
                        UpstreamFailure "setLocation failed because no locationParser was set"
            )
