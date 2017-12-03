module Elmer.SpyFakeTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Spy as Spy
import Elmer.Spy.Matchers exposing (..)
import Elmer.Platform.Command as Command
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.TestApps.SpyFakeTestApp as App
import Task

createWithTests : Test
createWithTests =
  describe "createWith"
  [ describe "when a fake function is used"
    [ test "it records the call" <|
      \() ->
        let
          spy =
            Spy.createWith "my-fake" (\tagger word ->
              Command.fake <| tagger word
            )
          dependencies =
            { fetchName = Spy.callable "my-fake"
            , getNumber = (\_ -> 33)
            }
        in
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ spy ]
            |> Markup.target "#fetch-name-button"
            |> Event.click
            |> Spy.expect "my-fake" (
              wasCalledWith [ functionArg, stringArg "Cool Dude" ]
            )
    ]
  , describe "when more than one fake function is used" <|
      let
          funSpy =
            Spy.createWith "fun-fake" (\tagger word ->
              Command.fake <| tagger word
            )
          awesomeSpy =
            Spy.createWith "awesome-fake" (\thing ->
              27
            )
          dependencies =
            { fetchName = Spy.callable "fun-fake"
            , getNumber = Spy.callable "awesome-fake"
            }
          state =
            Elmer.given App.initialModel App.view (App.update dependencies)
              |> Spy.use [ funSpy, awesomeSpy ]
              |> Markup.target "#fetch-name-button"
              |> Event.click
              |> Event.click
      in
        [ test "it records the call for the first" <|
          \() ->
            state
              |> Spy.expect "fun-fake" (
                wasCalledWith [ functionArg, stringArg "Cool Dude" ]
              )
        , test "it records the number of calls for the first spy" <|
          \() ->
            state
              |> Spy.expect "fun-fake" (
                wasCalled 2
              )
        , test "it records the call for the second" <|
          \() ->
            state
              |> Spy.expect "awesome-fake" (
                wasCalledWith [ stringArg "Cool Dude" ]
              )
        , test "it records the number of calls for the second spy" <|
          \() ->
            state
              |> Spy.expect "awesome-fake" (
                wasCalled 2
              )
        ]
  ]

callableTests : Test
callableTests =
  describe "callable"
  [ describe "when callable is used with a spy for a real function" <|
    let
      spy = Spy.create "task-spy" (\_ -> Task.perform)
        |> Spy.andCallFake (\_ _ -> Cmd.none)
      dependencies =
        { fetchName = Spy.callable "task-spy"
        , getNumber = (\_ -> 33)
        }
    in
      [ test "it allows you to call it" <|
        \() ->
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ spy ]
            |> Markup.target "#fetch-name-button"
            |> Event.click
            |> Spy.expect "task-spy" (
              wasCalledWith [ functionArg, stringArg "Cool Dude" ]
            )
      ]
  ]
