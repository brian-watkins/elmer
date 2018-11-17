module Elmer.SpyFakeTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Spy as Spy
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
            Spy.observe (\_ -> myFake)
              |> Spy.andCallThrough

          dependencies =
            { fetchName = Spy.inject (\_ -> myFake)
            , getNumber = (\_ -> 33)
            }
        in
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ fake ]
            |> Markup.target << by [ id "fetch-name-button" ]
            |> Event.click
            |> Spy.expect (\_ -> myFake) (
              wasCalledWith [ functionArg, stringArg "Cool Dude" ]
            )
    ]
  , describe "when more than one fake function is used" <|
      let
        funFake =
          Spy.observe (\_ -> myFake)
            |> Spy.andCallThrough
        
        awesomeFake =
          Spy.observe (\_ -> constantFake)
            |> Spy.andCallFake (\_ -> 17)

        dependencies =
          { fetchName = Spy.inject (\_ -> myFake)
          , getNumber = Spy.inject (\_ -> constantFake)
          }

        state =
          Elmer.given App.initialModel App.view (App.update dependencies)
            |> Spy.use [ funFake, awesomeFake ]
            |> Markup.target << by [ id "fetch-name-button" ]
            |> Event.click
            |> Event.click
      in
        [ test "it records the call for the first" <|
          \() ->
            state
              |> Spy.expect (\_ -> myFake) (
                wasCalledWith [ functionArg, stringArg "Cool Dude" ]
              )
        , test "it records the number of calls for the first spy" <|
          \() ->
            state
              |> Spy.expect (\_ -> myFake) (
                wasCalled 2
              )
        , test "it records the call for the second" <|
          \() ->
            state
              |> Spy.expect (\_ -> constantFake) (
                wasCalledWith [ stringArg "Cool Dude" ]
              )
        , test "it records the number of calls for the second spy" <|
          \() ->
            state
              |> Spy.expect (\_ -> constantFake) (
                wasCalled 2
              )
        ]
  ]


myFake tagger word =
  tagger word
    |> Command.fake


constantFake thing =
  0
