module Elmer.Navigation
    exposing
        ( navigationComponentState
        , setLocation
        , expectLocation
        , spy
        )

{-| Functions for describing the behavior of components that use
[elm-lang/navigation](http://package.elm-lang.org/packages/elm-lang/navigation/latest/Navigation).

# Create a ComponentState
@docs navigationComponentState

# Update the Location
@docs setLocation

# Make Expectations about the Location
@docs spy, expectLocation

-}

import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Spy.Internal as Spy_
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.Component exposing (Component)
import Elmer exposing (Matcher)
import Expect
import Elmer.Printer exposing (..)
import Elmer.Navigation.Location as Location
import Navigation
import Html exposing (Html)

{-| Create a `ComponentState` with a location parser function.

The location parser function is the function you would provide to
`Navigation.program` when you initialize your app.
-}
navigationComponentState
  :  model
  -> ( model -> Html msg )
  -> ( msg -> model -> ( model, Cmd msg ) )
  -> ( Navigation.Location -> msg )
  -> Elmer.ComponentState model msg
navigationComponentState model view update parser =
  ComponentState.create model view update
    |> ComponentState.map (\component ->
      ComponentState.with { component | locationParser = Just parser }
    )

{-| Stub `Navigation.newUrl` and `Navigation.modifyUrl` with a function that
records the location as it is set.

You must use this function with `Elmer.Spy.use` in order to make expectations
about the location.

Suppose you want to test a home button that sets the
location to `/home` when clicked:

    componentState
      |> Spy.use [ Navigation.spy ]
      |> Elmer.Html.find "#home-button"
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

generateCommandForLocation : String -> Component model msg -> Cmd msg
generateCommandForLocation url componentState =
  case componentState.locationParser of
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

storeLocation : String -> Component model msg -> Component model msg
storeLocation url componentState =
  { componentState | location = Just url }

{-| Expect that the current location is equal to the given string.

Note: This expectation must be used in conjunction with `spy` above.
-}
expectLocation : String -> Matcher (Elmer.ComponentState model msg)
expectLocation expectedURL =
  ComponentState.mapToExpectation <|
      \componentState ->
          case componentState.location of
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
setLocation : String -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
setLocation location =
  ComponentState.map (\component ->
    case component.locationParser of
        Just locationParser ->
          let
              commandThunk = \() -> fakeNavigateCommand location
          in
              Command.send commandThunk <| ComponentState.with component
        Nothing ->
            ComponentState.failure "setLocation failed because no locationParser was set"
  )
