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
  [ spyTests
  , expectSpyTests
  , calledTests
  , restoreTests
  , stubTests
  ]

spyTests : Test
spyTests =
  describe "spy"
  [ describe "when there is an upstream failure"
    [ test "it fails" <|
      \() ->
        let
          initialState = Failed "You failed!"
        in
          Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName) initialState
            |> Expect.equal (Failed "You failed!")
    ]
  , describe "when the argument does not reference a function"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
        in
          Elmer.Platform.spy "my-spy" (\_ -> "Huh?") initialState
            |> Expect.equal (Failed "Failed to install stubs!")
    ]
  , describe "when the argument references a function"
    [ describe "when the function is called"
      [ test "it still functions normally" <|
        \() ->
          Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
    [ test "it sets the name on the spyData and passes it to the matcher" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
          |> Markup.find "#button"
          |> Event.click
          |> Elmer.Platform.expectSpy "clearName" (\spy ->
              Expect.equal spy.name "clearName"
            )
    , test "it sets the number of calls on the spyData and passes it to the matcher" <|
      \() ->
        Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
          |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
          |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
            |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
            |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
            |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
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
          |> Elmer.Platform.spy "clearName" (\_ -> SpyApp.clearName)
          |> Elmer.Platform.expectSpy "clearName" (Matchers.wasCalled 0)
    , test "it gets cleared before the next test" <|
      \() ->
        Expect.equal (Native.Platform.spyData "clearName") Nothing
    ]
  , describe "when a stub is used"
    [ test "the stub is set" <|
      \() ->
        let
          stub = Elmer.Platform.stub (\_ -> SpyApp.titleText) (\_ -> "Test Title")
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
  [ describe "when there is an upstream failure"
    [ test "it fails" <|
      \() ->
        let
          initialState = Failed "You failed!"
          stub = Elmer.Platform.stub (\_ -> SpyApp.titleText) (\_ -> "Test Title")
        in
          Elmer.Platform.use [ stub ] initialState
            |> Expect.equal (Failed "You failed!")
    ]
  , describe "when the argument does not reference a function"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
          stub = Elmer.Platform.stub (\_ -> "Huh?") (\_ -> "Test Title")
        in
           Elmer.Platform.use [ stub ] initialState
            |> Expect.equal (Failed "Failed to install stubs!")
    ]
  , describe "when the argument references a function"
    [ describe "when the function is called"
      [ test "it calls the mocked version" <|
        \() ->
          let
            stub = Elmer.Platform.stub (\_ -> SpyApp.titleText) (\_ -> "Test Title")
          in
            Elmer.componentState SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Elmer.Platform.use [ stub ]
              |> Markup.find "#title"
              |> Markup.expectElement (hasText "Test Title")
      ]
    , describe "when a command is sent"
      [ test "it uses the mocked version" <|
        \() ->
          Elmer.componentState HttpApp.defaultModel HttpApp.view HttpApp.update
            |> Elmer.Platform.use [ Elmer.Http.spy ]
            |> Command.send (\() -> HttpApp.sendRequest HttpApp.defaultModel)
            |> Elmer.Http.expectGET "http://fun.com/fun.html" HttpMatchers.hasBeenRequested
      ]
    ]
  ]
