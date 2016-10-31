module Elmer.BrowserTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Expect
import Elmer exposing (..)
import Elmer.Event as Event
import Elmer.Browser as Browser

all : Test
all =
  describe "Browser Tests"
    [ expectLocationTests
    ]

expectLocationTests =
  describe "location tests"
  [ describe "when there is an upstream failure"
    [ test "it reports the failure" <|
      \() ->
        let
          initialState = UpstreamFailure "Nothing found!"
        in
          Browser.expectLocation "http://blah.com" initialState
            |> Expect.equal (Expect.fail "Nothing found!")
    ]
  , describe "when a location has not been set"
    [ test "it explains the failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find "#navigationClick" initialState
            |> Browser.expectLocation "http://badplace.com"
            |> Expect.equal
              (Expect.fail "Expected to be at location:\n\n\thttp://badplace.com\n\nbut no location has been set")
    ]
  , describe "when a newURL command is sent"
    [ describe "when the correct url is expected"
      [ test "it passes" <|
          \() ->
            let
              initialState = Elmer.componentState App.defaultModel App.view App.update
            in
              Elmer.find "#navigationClick" initialState
                |> Event.click
                |> Browser.expectLocation "http://fun.com/fun.html"
                |> Expect.equal Expect.pass
      ]
    , describe "when the incorrect url is expected"
      [ describe "when a location has been set"
        [ test "it explains the failure" <|
          \() ->
            let
              initialState = Elmer.componentState App.defaultModel App.view App.update
            in
              Elmer.find "#navigationClick" initialState
                |> Event.click
                |> Browser.expectLocation "http://badplace.com"
                |> Expect.equal
                  (Expect.fail "Expected to be at location:\n\n\thttp://badplace.com\n\nbut location is:\n\n\thttp://fun.com/fun.html")
        ]
      ]
    ]
  , describe "when a modifyUrl command is sent"
    [ describe "when the correct url is expected"
      [ test "it passes" <|
        \() ->
          let
            initialState = Elmer.componentState App.defaultModel App.view App.update
          in
            Elmer.find "#modifyNavigationClick" initialState
              |> Event.click
              |> Browser.expectLocation "http://fun.com/evenMoreFun.html"
              |> Expect.equal Expect.pass
      ]
    ]
  ]
