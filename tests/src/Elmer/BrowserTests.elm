module Elmer.BrowserTests exposing (all)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Program
import Elmer.Spy as Spy
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Command as Command
import Elmer.Html
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Message exposing (..)
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
          Elmer.Program.init (\() -> (InitApp.defaultModel "", Cmd.none)) initialState
            |> Expect.equal (TestState.failure "You failed!")
    ]
  , describe "when there is no failure" <|
    let
      taskSpy =
        Spy.observe (\_ -> InitApp.requestTokenTask)
          |> Spy.andCallFake (\_ -> Task.succeed "Spy Token!")

      state = Elmer.Program.givenElement InitApp.view InitApp.update
        |> Spy.use [ taskSpy ]
        |> Elmer.Program.init (\() -> InitApp.init { baseUrl = "http://fun.com/api" })
    in
    [ test "it sets the model" <|
      \() ->
        state
          |> Elmer.Html.target << by [ Sel.id "base-url" ]
          |> Elmer.Html.expect (element <| hasText "http://fun.com/api")
    , test "it sends the command" <|
      \() ->
        state
          |> Elmer.expectModel (\model ->
            Expect.equal "Spy Token!" model.token
          )
    ]
  , describe "when the command fails"
    [ test "it fails" <|
      \() ->
        let
          state = 
            Elmer.Program.givenElement InitApp.view InitApp.update
              |> Elmer.Program.init ( \() -> 
                ( InitApp.defaultModel ""
                , Command.fail "Failed!"
                )
              )
        in
          Expect.equal state (TestState.failure <|
            format
              [ note "Failed!"
              ]
          )
    ]
  ]

