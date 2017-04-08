module Elmer.PlatformTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.TestApps.SpyTestApp as SpyApp
import Elmer.TestApps.HttpTestApp as HttpApp
import Elmer.Internal exposing (..)
import Elmer.Platform
import Elmer.Platform.Matchers as Matchers
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Matchers exposing (hasText)
import Elmer.Printer exposing (..)
import Elmer
import Elmer.Http
import Elmer.Http.Matchers as HttpMatchers
import Elmer.Platform.Command as Command

all : Test
all =
  describe "Platform Tests"
  [ useTests
  , spyTests
  , expectSpyTests
  , calledTests
  , restoreTests
  , stubTests
  ]

useTests : Test
useTests =
  describe "use"
  [ describe "when there is an upstream failure"
    [ test "it fails" <|
      \() ->
        let
          initialState = Failed "You failed!"
          spy = Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
        in
          Elmer.Platform.use [ spy ] initialState
            |> Expect.equal (Failed "You failed!")
    ]
  ]

spyTests : Test
spyTests =
  describe "spy"
  [ describe "when the argument does not reference a function"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          spy = Elmer.Platform.spy "my-spy" (\_ -> "Huh?")
        in
          Elmer.Platform.use [ spy ] initialState
            |> Expect.equal (Failed "Failed to install stubs!")
    ]
  , describe "when the argument references a function"
    [ describe "when the function is called"
      [ test "it still functions normally" <|
        \() ->
          let
            spy = Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
          in
            Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Elmer.Platform.use [ spy ]
              |> Markup.find "#button"
              |> Event.click
              |> Elmer.expectModel (\model ->
                  Expect.equal model.name (Just "Default Name")
                )
      ]
    ]
  ]

expectSpyTests : Test
expectSpyTests =
  describe "expectSpy"
  [ describe "when there is a failure upstream"
    [ test "it fails" <|
      \() ->
        Failed "Upstream Failure"
          |> Elmer.Platform.expectSpy "some-spy" (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail "Upstream Failure")
    ]
  , describe "when the function has not been registered as a spy"
    [ test "it fails" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.expectSpy "some-spy" (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail (format
            [ message "Attempted to make expectations about a spy" "some-spy"
            , description "but it has not been registered as a spy"
            ]
          ))
    ]
  , describe "when the function has been registered as a spy"
    [ test "it sets the name and passes it to the matcher" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
          |> Markup.find "#button"
          |> Event.click
          |> Elmer.Platform.expectSpy "clearName" (\spy ->
              Expect.equal spy.name "clearName"
            )
    , test "it sets the number of calls and passes it to the matcher" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
          |> Markup.find "#button"
          |> Event.click
          |> Event.click
          |> Event.click
          |> Elmer.Platform.expectSpy "clearName" (\spy ->
              Expect.equal spy.calls 3
            )
    ]
  ]

calledTests : Test
calledTests =
  describe "called matcher"
  [ describe "when the spy has not been called"
    [ test "it fails with the message" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
          |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 2)
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy clearName to have been called" "2 times"
              , message "but it was called" "0 times"
              ]
            )
    , test "it fails with a properly depluralized message" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
          |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 1)
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy clearName to have been called" "1 time"
              , message "but it was called" "0 times"
              ]
            )
    ]
  , describe "when the spy has been called"
    [ describe "when the expected count does not match the number of calls"
      [ test "it fails" <|
        \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
            |> Markup.find "#button"
            |> Event.click
            |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 2)
            |> Expect.equal (Expect.fail <|
              format
                [ message "Expected spy clearName to have been called" "2 times"
                , message "but it was called" "1 time"
                ]
              )
      , test "it fails" <|
        \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
            |> Markup.find "#button"
            |> Event.click
            |> Event.click
            |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 3)
            |> Expect.equal (Expect.fail <|
              format
                [ message "Expected spy clearName to have been called" "3 times"
                , message "but it was called" "2 times"
                ]
              )
      ]
    , describe "when the expected count matches the number of calls"
      [ test "it passes" <|
        \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
            |> Markup.find "#button"
            |> Event.click
            |> Event.click
            |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 2)
            |> Expect.equal (Expect.pass)
      ]
    ]
  ]

restoreTests : Test
restoreTests =
  describe "restore"
  [ describe "when a spy is registered"
    [ test "it gets set by Elmer.Platform.spy" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.use [ Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) ]
          |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 0)
    , test "it gets cleared before the next test" <|
      \() ->
        Expect.equal (Native.Platform.callsForSpy "clearName") Nothing
    ]
  , describe "when a stub is used"
    [ test "the stub is set" <|
      \() ->
        let
          stub = Elmer.Platform.spy "my-spy" (\_ -> SpyApp.titleText)
            |> Elmer.Platform.andCallFake (\_ -> "Test Title")
        in
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.use [ stub ]
            |> Markup.find "#title"
            |> Markup.expectElement (hasText "Test Title")
    , test "it gets cleared before the next test" <|
      \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Markup.find "#title"
            |> Markup.expectElement (hasText "A Title: Some Title")
    ]
  ]

stubTests : Test
stubTests =
  describe "stub"
  [ describe "when the argument references a function"
    [ let
        stub = Elmer.Platform.spy "titleText" (\_ -> SpyApp.titleText)
          |> Elmer.Platform.andCallFake (\_ -> "Test Title")

        stateThunk = \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.use [ stub ]
            |> Markup.find "#title"
      in
        describe "when the function is called"
          [ test "it calls the mocked version" <|
            \() ->
              Markup.find "#title" (stateThunk ())
                |> Markup.expectElement (hasText "Test Title")
          , test "it records the call" <|
            \() ->
              Elmer.Platform.expectSpy "titleText" (Matchers.wasCalled 1) (stateThunk ())
          ]
    ]
  ]
