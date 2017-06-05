module Elmer.Navigation
    exposing
        ( withLocationParser
        , setLocation
        , expectLocation
        , spy
        )

{-| Functions for describing the behavior of components that use
[elm-lang/navigation](http://package.elm-lang.org/packages/elm-lang/navigation/latest/Navigation).

# Register the location parser
@docs withLocationParser

# Update the Location
@docs setLocation

# Make Expectations about the Location
@docs spy, expectLocation

-}

import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Spy.Internal as Spy_
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Context exposing (Context)
import Elmer exposing (Matcher)
import Expect
import Elmer.Printer exposing (..)
import Elmer.Navigation.Location as Location
import Navigation
import Html exposing (Html)


{-| Register a location parser with the current test context.

The location parser function is the function you would provide to
`Navigation.program` when you initialize your app.
-}
withLocationParser : ( Navigation.Location -> msg ) -> Elmer.TestState model msg -> Elmer.TestState model msg
withLocationParser parser =
  TestState.map (\context ->
    TestState.with { context | locationParser = Just parser }
  )

{-| Stub `Navigation.newUrl` and `Navigation.modifyUrl` with a function that
records the location as it is set.

You must use this function with `Elmer.Spy.use` in order to make expectations
about the location.

Suppose you want to test a home button that sets the
location to `/home` when clicked:

    testState
      |> Spy.use [ Navigation.spy ]
      |> Elmer.Html.target "#home-button"
      |> Elmer.Html.Event.click
      |> Navigation.expectLocation "/home"

-}
spy : Spy
spy =
  Spy_.batch
    [ Spy.create "Navigation.newUrl" (\_ -> Navigation.newUrl)
        |> andCallFake fakeNavigateCommand
    , Spy.create "Navigation.modifyUrl" (\_ -> Navigation.modifyUrl)
        |> andCallFake fakeNavigateCommand
    ]

fakeNavigateCommand : String -> Cmd msg
fakeNavigateCommand url =
  let
    parseCommand = Platform.generateCommand <| generateCommandForLocation url
    stateCommand = Platform.mapStateCommand <| storeLocation url
  in
    Cmd.batch [ stateCommand, parseCommand ]

generateCommandForLocation : String -> Context model msg -> Cmd msg
generateCommandForLocation url context =
  case context.locationParser of
    Just locationParser ->
      let
        message = handleLocationUpdate url locationParser
      in
        Command.fake message
    Nothing ->
      Cmd.none

handleLocationUpdate : String -> (Navigation.Location -> msg) -> msg
handleLocationUpdate url parser =
    (parser (Location.asLocation url))

storeLocation : String -> Context model msg -> Context model msg
storeLocation url testState =
  { testState | location = Just url }

{-| Expect that the current location is equal to the given string.

Note: This expectation must be used in conjunction with `spy` above.
-}
expectLocation : String -> Matcher (Elmer.TestState model msg)
expectLocation expectedURL =
  TestState.mapToExpectation <|
      \testState ->
          case testState.location of
              Just location ->
                  Expect.equal location expectedURL
                      |> Expect.onFail (format [message "Expected to be at location:" expectedURL, message "but location is:" location])

              Nothing ->
                  Expect.fail (format [message "Expected to be at location:" expectedURL, description "but no location has been set"])

{-| Set the location for the component.

When the location is set and a location parser is defined for this component,
then the parser will be applied to the location and the resulting message
will be passed to the component's `update` function for processing.
-}
setLocation : String -> Elmer.TestState model msg -> Elmer.TestState model msg
setLocation location =
  TestState.map (\context ->
    case context.locationParser of
        Just locationParser ->
          let
              commandThunk = \() -> fakeNavigateCommand location
          in
              Command.send commandThunk <| TestState.with context
        Nothing ->
            TestState.failure "setLocation failed because no locationParser was set"
  )
