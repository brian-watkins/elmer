module Elmer.Navigation
    exposing
        ( fakeKey
        , expectLocation
        , spy
        )

{-| Functions for describing the navigation behavior of Elm Html programs created with `Browser.application`.

# Make Expectations about the Current Location
@docs spy, expectLocation

# Get a Fake Navigation Key
@docs fakeKey

-}

import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Spy.Internal as Spy_
import Elmer.Platform.Command as Command
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Context as Context exposing (Context)
import Elmer.Errors as Errors exposing (failWith)
import Elmer exposing (Matcher)
import Elmer.Value as Value
import Elmer.Navigation.Internal exposing (..)
import Expect
import Elmer.Printer exposing (..)
import Browser.Navigation as Navigation
import Html exposing (Html)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


{-| Generate a fake `Browser.Navigation.Key` value.

Use this value when calling the init function of a `Browser.application` program during a test.
-}
fakeKey : Navigation.Key
fakeKey =
  Value.encode (Value.for "Key") []


{-| Stub `Browser.Navigation.pushUrl` and `Browser.Navigation.replaceUrl` with a function that
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
    [ Spy.create "Navigation.pushUrl" (\_ -> Browser.Navigation.pushUrl)
        |> andCallFake (fakeNavigateCommand "Browser.Navigation.pushUrl")
    , Spy.create "Navigation.replaceUrl" (\_ -> Browser.Navigation.replaceUrl)
        |> andCallFake (fakeNavigateCommand "Browser.Navigation.replaceUrl")
    ]


generateUrlChangeCommand : String -> String -> Context model msg -> Cmd msg
generateUrlChangeCommand functionName urlString context =
  case Context.state NavigationTaggers context of
    Just { onUrlRequest, onUrlChange } ->
      case Url.fromString urlString of
        Just url ->
          Command.fake <| onUrlChange url
        Nothing ->
          Command.fail <| Errors.print <| Errors.badUrl functionName urlString
    Nothing ->
      Command.fail <| Errors.print <| Errors.navigationSpyRequiresApplication functionName urlString


fakeNavigateCommand : String -> Key -> String -> Cmd msg
fakeNavigateCommand functionName _ url =
  let
    parseCommand = RuntimeCommand.generate <| generateUrlChangeCommand functionName url
    stateCommand = RuntimeCommand.mapState Location (\_ -> url)
  in
    Cmd.batch [ stateCommand, parseCommand ]


{-| Expect that the current location is equal to the given string.

This expectation must be used in conjunction with `spy` above, and your `TestState` must be 
created with `Elmer.Application.given`. 

Suppose your app calls `Browser.Navigation.pushUrl` when an element is clicked. You can describe
this behavior as follows: 

    Elmer.Application.given App.OnUrlRequest App.OnUrlChange App.view App.update
      |> Elmer.Spy.use [ Elmer.Navigation.spy ]
      |> Elmer.init (\_ -> App.init testFlags testUrl Elmer.Navigation.fakeKey)
      |> Elmer.Html.target "#some-element"
      |> Elmer.Html.Event.click
      |> Elmer.Navigation.expectLocation "http://mydomain.com/funStuff.html"

Note that `expectLocation` will only match on urls provided via `Browser.Navigation.pushUrl` or
`Browser.Navigation.replaceUrl`. In particular, `expectLocation` will not match the url provided
as part of the call to `Elmer.init` that provides the initial model and command values. 

-}
expectLocation : String -> Matcher (Elmer.TestState model msg)
expectLocation expectedURL =
  TestState.mapToExpectation <|
      \context ->
        case Context.state Location context of
          Just location ->
            Expect.equal location expectedURL
                |> Expect.onFail (Errors.print <| Errors.wrongLocation expectedURL location)
          Nothing ->
            failWith <| Errors.noLocation expectedURL
