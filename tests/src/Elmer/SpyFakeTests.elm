module Elmer.SpyFakeTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Spy as Spy exposing (this)
import Elmer.Spy.Matchers exposing (..)
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Html.Event as Event
import Elmer.TestApps.SpyFakeTestApp as App
import Task


all : Test
all =
  Test.concat
  [ createWithTests
  ]


createWithTests : Test
createWithTests =
  describe "createWith"
  [ describe "when a fake function is used"
    [ test "it records the call" <|
      \() ->
        let
          fake =
            Spy.onFake "my-fake" (\tagger word ->
              Command.fake <| tagger word
            )
          dependencies =
            { fetchName = Spy.callable fake
            , getNumber = (\_ -> 33)
            }
        in
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ this fake ]
            |> Markup.target << by [ id "fetch-name-button" ]
            |> Event.click
            |> Spy.expect "my-fake" (
              wasCalledWith [ functionArg, stringArg "Cool Dude" ]
            )
    ]
  , describe "when more than one fake function is used" <|
      let
        funFake =
          Spy.onFake "fun-fake" (\tagger word ->
            Command.fake <| tagger word
          )
        awesomeFake =
          Spy.onFake "awesome-fake" (\thing -> 17)
        dependencies =
          { fetchName = Spy.callable funFake
          , getNumber = Spy.callable awesomeFake
          }
        state =
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ this funFake, this awesomeFake ]
            |> Markup.target << by [ id "fetch-name-button" ]
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
