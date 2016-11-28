module Elmer.NavigationTests exposing (all)

import Test exposing (..)
import Elmer.TestApp as App
import Expect
import Elmer exposing (..)
import Elmer.Types exposing (..)
import Elmer.Event as Event
import Elmer.Navigation as ElmerNav
import Elmer.Navigation.Location as Location
import Elmer.Matchers as Matchers
import Elmer.TestExtra as ElmerTest
import Navigation

all : Test
all =
  describe "Navigation Tests"
    [ expectLocationTests
    , setLocationTests
    , asLocationTests
    ]

expectLocationTests =
  describe "location tests"
  [ describe "when there is an upstream failure"
    [ test "it reports the failure" <|
      \() ->
        let
          initialState = UpstreamFailure "Nothing found!"
        in
          ElmerNav.expectLocation "http://blah.com" initialState
            |> Expect.equal (Expect.fail "Nothing found!")
    ]
  , describe "when a location has not been set"
    [ test "it explains the failure" <|
      \() ->
        let
          initialState = Elmer.componentState App.defaultModel App.view App.update
        in
          Elmer.find "#navigationClick" initialState
            |> ElmerNav.expectLocation "http://badplace.com"
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
                |> ElmerNav.expectLocation "http://fun.com/fun.html"
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
                |> ElmerNav.expectLocation "http://badplace.com"
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
              |> ElmerNav.expectLocation "http://fun.com/evenMoreFun.html"
              |> Expect.equal Expect.pass
      ]
    ]
  ]

setLocationTests =
  let
    fullState = navigationComponentState App.defaultModel App.view App.update App.parseLocation
  in
  describe "set location"
  [ describe "when there is an upstream failure"
    [ test "it shows the message" <|
      \() ->
        let
          failureState = UpstreamFailure "failed"
        in
          ElmerNav.setLocation "http://fun.com/fun.html" failureState
            |> Elmer.find ".error"
            |> Elmer.expectNodeExists
            |> Expect.equal (Expect.fail "failed")
    ]
  , describe "when no parser is set"
    [ test "it fails with a message" <|
      \() ->
        let
          stateWithoutParser = Elmer.map (\s -> CurrentState { s | locationParser = Nothing }) fullState
        in
          ElmerNav.setLocation "http://fun.com/fun.html" stateWithoutParser
            |> Elmer.find ".error"
            |> Elmer.expectNodeExists
            |> Expect.equal (Expect.fail "setLocation failed because no locationParser was set")
    ]
  , describe "when locationParser is set"
    [ test "it updates the component state with the new location" <|
      \() ->
        let
          initialState = Elmer.map (\s -> CurrentState { s | locationParser = Just App.parseLocationFail }) fullState
        in
          ElmerNav.setLocation "http://fun.com/fun.html" initialState
            |> Elmer.find ".error"
            |> Elmer.expectNode (Matchers.hasText "Route not found: Unparseable url!")
            |> Expect.equal Expect.pass
    , test "it updates the component state with another location" <|
      \() ->
        ElmerNav.setLocation "http://fun.com/api/view" fullState
          |> Elmer.find ".error"
          |> Elmer.expectNodeExists
          |> ElmerTest.not
          |> Expect.equal Expect.pass
    ]
  ]

asLocationTests =
  describe "asLocation tests"
  [ describe "hash"
    [ test "it has a default value" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html"
        in
          Expect.equal location.hash ""
    , test "it finds the hashed value" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.hash "#some/fun/path"
    , describe "when the hash is relative"
      [ test "it finds the hashed value" <|
        \() ->
          let
            location = Location.asLocation "#some/fun/path"
          in
            Expect.equal location.hash "#some/fun/path"
      , test "it finds a single hashed value" <|
        \() ->
          let
            location = Location.asLocation "#hash"
          in
            Expect.equal location.hash "#hash"
      ]
    ]
  , describe "path"
    [ test "it finds the path" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.pathname "/fun.html?key=value#some/fun/path"
    ]
  , describe "href"
    [ test "it contains the url" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.href "http://fun.com/fun.html?key=value#some/fun/path"
    ]
  , describe "protocol"
    [ test "it contains the protocol" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.protocol "http:"
    , test "it has a default value" <|
      \() ->
        let
          location = Location.asLocation "/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.protocol ""
    ]
  , describe "search"
    [ test "it gets the query string" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value#some/fun/path"
        in
          Expect.equal location.search "?key=value"
    , test "it gets the query string when there is no hash" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html?key=value"
        in
          Expect.equal location.search "?key=value"
    , test "it returns an empty string when there is no query string" <|
      \() ->
        let
          location = Location.asLocation "http://fun.com/fun.html"
        in
          Expect.equal location.search ""
        ]
  ]
