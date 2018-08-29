module Elmer.BrowserTests exposing (all)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Browser
import Elmer.Spy as Spy
import Elmer.Http
import Elmer.Http.Route as Route
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Html
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Printer exposing (..)
import Elmer.TestApps.InitTestApp as InitApp
import Task
import Time


all : Test
all =
  Test.concat
  [ initTests
  ]


initTests : Test
initTests =
  describe "init"
  [ describe "when there is a faiure"
    [ test "it fails" <|
      \() ->
        let
          initialState = TestState.failure "You failed!"
        in
          Elmer.Browser.init (\() -> (InitApp.defaultModel "", Cmd.none)) initialState
            |> Expect.equal (TestState.failure "You failed!")
    ]
  , let
      state = Elmer.given (InitApp.defaultModel "") InitApp.view InitApp.update
        |> Spy.use [ Elmer.Http.spy ]
        |> Elmer.Browser.init (\() -> InitApp.init { baseUrl = "http://fun.com/api" })
    in
      describe "when there is no failure"
      [ test "it sets the model" <|
        \() ->
          state
            |> Elmer.Html.target "#base-url"
            |> Elmer.Html.expect (element <| hasText "http://fun.com/api")
      , test "it sends the command" <|
        \() ->
          state
            |> Elmer.Http.expect (Route.get "http://fun.com/api/token")
      ]
  , describe "when the command fails"
    [ test "it fails" <|
      \() ->
        let
          state = 
            Elmer.given (InitApp.defaultModel "") InitApp.view InitApp.update
              |> Elmer.Browser.init ( \() -> 
                ( InitApp.defaultModel ""
                , Task.perform InitApp.Tag ( Time.now |> Task.map ( \p -> Time.posixToMillis p |> String.fromInt ) )
                )
              )
        in
          Expect.equal state (TestState.failure <|
            format
              [ description "Encountered a native task.\nStub any task-generating functions with Task.succeed or Task.fail as necessary."
              ]
          )
    ]
  ]

