module Elmer.SpyTests exposing (..)

import Test exposing (..)
import Expect
import Elmer.TestApps.SpyTestApp as SpyApp
import Elmer.TestApps.HttpTestApp as HttpApp
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Spy as Spy
import Elmer.Spy.Arg exposing (Arg(..))
import Elmer.Spy.Matchers as Matchers
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Printer exposing (..)
import Elmer
import Elmer.Http
import Elmer.Http.Matchers as HttpMatchers
import Elmer.Platform.Command as Command


all : Test
all =
  Test.concat
  [ useTests
  , spyTests
  , expectSpyTests
  , spyArgumentTests
  , restoreTests
  , andCallFakeTests
  , replaceValueTests
  ]


useTests : Test
useTests =
  describe "use"
  [ describe "when there is an upstream failure"
    [ test "it fails" <|
      \() ->
        let
          initialState = TestState.failure "You failed!"
          spy = Spy.create "clearName" (\_ -> SpyApp.clearName)
        in
          Spy.use [ spy ] initialState
            |> Expect.equal (TestState.failure "You failed!")
    ]
  ]

spyTests : Test
spyTests =
  describe "spy"
  [ describe "when the argument does not reference a function"
    [ test "it fails" <|
      \() ->
        let
          initialState = Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          spy = Spy.create "my-spy" (\_ -> "Huh?")
        in
          Spy.use [ spy ] initialState
            |> Expect.equal (TestState.failure <|
              format
                [ message "Failed to activate spies" "my-spy"
                ]
            )
    ]
  , describe "when the argument references a function"
    [ describe "when the function is called"
      [ test "it still functions normally" <|
        \() ->
          let
            spy = Spy.create "clearName" (\_ -> SpyApp.clearName)
          in
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ spy ]
              |> Markup.target "#button"
              |> Event.click
              |> Elmer.expectModel (\model ->
                  Expect.equal model.name "Default Name"
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
        TestState.failure "Upstream Failure"
          |> Spy.expect "some-spy" (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail "Upstream Failure")
    ]
  , describe "when the function has not been registered as a spy"
    [ test "it fails" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.expect "some-spy" (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail (format
            [ message "Attempted to make expectations about a spy" "some-spy"
            , description "but it has not been registered as a spy"
            ]
          ))
    ]
  , describe "when the function has been registered as a spy"
    [ describe "when the function has only one argument"
      [ test "it sets the name and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ Spy.create "titleText" (\_ -> SpyApp.titleText) ]
            |> Markup.target "#title"
            |> Spy.expect "titleText" (\spy ->
                Expect.equal spy.name "titleText"
              )
      , test "it sets the number of calls and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ Spy.create "titleText" (\_ -> SpyApp.titleText) ]
            |> Markup.target "#title"
            |> Markup.render
            |> Spy.expect "titleText" (\spy ->
                Expect.equal (List.length spy.calls) 1
              )
      , test "it sets the arguments for each call and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ Spy.create "titleText" (\_ -> SpyApp.titleText) ]
            |> Markup.target "#title"
            |> Markup.render
            |> Spy.expect "titleText" (\spy ->
                Expect.equal spy.calls [ [ StringArg "Some Title" ] ]
              )
      , describe "when the spy calls a fake"
        [ test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            let
              spy =
                Spy.create "titleText" (\_ -> SpyApp.titleText)
                  |> Spy.andCallFake ( \_ -> "Fake Title" )
            in
              Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                |> Spy.use [ spy ]
                |> Markup.target "#title"
                |> Markup.render
                |> Spy.expect "titleText" (\actual ->
                    Expect.equal actual.calls [ [ StringArg "Some Title" ] ]
                  )
        , test "it calls the fake" <|
          \() ->
            let
              spy =
                Spy.create "titleText" (\_ -> SpyApp.titleText)
                  |> Spy.andCallFake ( \_ -> "Fake Title" )
            in
              Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                |> Spy.use [ spy ]
                |> Markup.target "#title"
                |> Markup.expect (element <| hasText "Fake Title")
        ]
      ]
    , describe "when the function has multiple arguments"
      [ describe "when all the arguments are provided at once"
        [ test "it sets the name and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#multi-arg-button"
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal spy.name "combineNames"
                )
        , test "it sets the number of calls and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#multi-arg-button"
              |> Event.click
              |> Event.click
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal (List.length spy.calls) 3
                )
        , test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#multi-arg-button"
              |> Event.click
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal spy.calls [ [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ], [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ] ]
                )
        , describe "when the spy calls a fake"
          [ test "it sets the arguments for each call and passes it to the matcher" <|
            \() ->
              let
                spy =
                  Spy.create "combineNames" (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Name" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target "#multi-arg-button"
                  |> Event.click
                  |> Event.click
                  |> Spy.expect "combineNames" (\actual ->
                      Expect.equal actual.calls [ [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ], [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ] ]
                    )
          , test "it calls the fake" <|
            \() ->
              let
                spy =
                  Spy.create "combineNames" (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Name" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target "#multi-arg-button"
                  |> Event.click
                  |> Markup.target "#name"
                  |> Markup.expect (element <| hasText "Name: Fake Name")
          ]
        ]
      , describe "when arguments are provided successively"
        [ test "it sets the name and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#successive-arg-button"
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal spy.name "combineNames"
                )
        , test "it sets the number of calls and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#successive-arg-button"
              |> Event.click
              |> Event.click
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal (List.length spy.calls) 3
                )
        , test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ Spy.create "combineNames" (\_ -> SpyApp.combineNames) ]
              |> Markup.target "#successive-arg-button"
              |> Event.click
              |> Event.click
              |> Spy.expect "combineNames" (\spy ->
                  Expect.equal spy.calls [ [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ], [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ] ]
                )
        , describe "when the spy calls a fake"
          [ test "it sets the arguments for each call and passes it to the matcher" <|
            \() ->
              let
                spy =
                  Spy.create "combineNames" (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Stuff" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target "#successive-arg-button"
                  |> Event.click
                  |> Event.click
                  |> Spy.expect "combineNames" (\actual ->
                      Expect.equal actual.calls [ [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ], [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ] ]
                    )
          , test "it calls the fake" <|
            \() ->
              let
                spy =
                  Spy.create "combineNames" (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Stuff" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target "#successive-arg-button"
                  |> Event.click
                  |> Markup.target "#name"
                  |> Markup.expect (element <| hasText "Name: Fake Stuff")
          ]
        ]
      ]
    ]
  ]


spyArgumentTests =
  describe "Spy Arguments"
  [ test "the call contains the correct types for each argument" <|
    \() ->
      let
        spy =
          Spy.create "fake-makeModel" (\_ -> SpyApp.makeModel)
      in
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.use [ spy ]
          |> Elmer.init (\_ ->
              SpyApp.init
                { name = "test-name"
                , times = 23
                , floatArg = 23.45
                , boolArg = True
                , recordArg = { kind = "Flowers", duration = 77.3 }
                , unionTypeArg = SpyApp.Fruit "Apple"
                , unionTypeTagger = SpyApp.Game
                }
            )
          |> Spy.expect "fake-makeModel" (\actual ->
              Expect.equal actual.calls
                [ [ StringArg "test-name"
                  , IntArg 23
                  , FloatArg 23.45
                  , BoolArg True
                  , TypedArg <| Elm.Kernel.Value.cast { kind = "Flowers", duration = 77.3 }
                  , TypedArg <| (Elm.Kernel.Value.cast <| SpyApp.Fruit "Apple")
                  , FunctionArg
                  ]
                ]
            )
  ]

restoreTests : Test
restoreTests =
  describe "restore"
  [ describe "when a spy is used"
    [ test "the spy is set" <|
      \() ->
        let
          stub = Spy.create "my-spy" (\_ -> SpyApp.titleText)
            |> Spy.andCallFake (\_ -> "Test Title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Markup.target "#title"
            |> Markup.expect (element <| hasText "Test Title")
    , test "it is not active for the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target "#title"
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  , describe "when a spy is used multiple times in the same test"
    [ test "the spy is set each time" <|
      \() ->
        let
          stub =
            Spy.create "my-spy" (\_ -> SpyApp.titleText)
              |> Spy.andCallFake (\_ -> "Test Title")
          anotherStub =
            Spy.create "my-spy" (\_ -> SpyApp.titleText)
              |> Spy.andCallFake (\_ -> "Another test title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Spy.use [ anotherStub ]
            |> Markup.target "#title"
            |> Markup.expect (element <| hasText "Another test title")
    , test "the spy is not set in the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target "#title"
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  , describe "when a component state map results in a failure"
    [ test "the spy is set" <|
      \() ->
        let
          stub = Spy.create "my-spy" (\_ -> SpyApp.titleText)
            |> Spy.andCallFake (\_ -> "Test Title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Markup.target "#title"
            |> Event.click
            |> Expect.equal (TestState.failure "No event handlers found for any of the triggered events: click, mousedown, mouseup, submit")
    , test "the spy is not active for the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target "#title"
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  ]

andCallFakeTests : Test
andCallFakeTests =
  describe "andCallFake"
  [ describe "when a fake function is specified"
    [ let
        spy = Spy.create "titleText" (\_ -> SpyApp.titleText)
          |> Spy.andCallFake (\_ -> "Test Title")

        state =
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Markup.target "#title"
      in
        describe "when the spied on function is called"
          [ test "it calls the fake version" <|
            \() ->
              state
                |> Markup.expect (element <| hasText "Test Title")
          , test "it records the call" <|
            \() ->
              Markup.render state
                |> Spy.expect "titleText" (Matchers.wasCalled 1)
          ]
    ]
  ]


replaceValueTests : Test
replaceValueTests =
  describe "replaceValue"
  [ describe "when a function with no arguments is replaced"
    [ test "it returns the fake value" <|
      \() ->
        let
          spy =
            "I am a fake footer"
              |> Spy.replaceValue (\_ -> SpyApp.footerText)
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Markup.target "#footer"
            |> Markup.expect (element <| hasText "I am a fake footer")
    ]
  , describe "when a function with arguments is replaced"
    [ test "it results in an error" <|
      \() ->
        let
          spy =
            "I am a fake title"
              |> Spy.replaceValue (\_ -> SpyApp.titleText)
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Markup.target "#footer"
            |> Markup.expect (element <| hasText "I am a fake footer")
            |> Expect.equal (Expect.fail <|
                format
                  [ message "Failed to activate spies" "titleText"
                  ]
              )
    ]
  , describe "when attempt to replace something that is not a function"
    [ test "it fails" <|
      \() ->
        let
          spy =
            "I am a fake title"
              |> Spy.replaceValue (\_ -> "not a spyable function")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Markup.target "#footer"
            |> Markup.expect (element <| hasText "I am a fake footer")
            |> Expect.equal (Expect.fail <|
                format
                  [ message "Failed to activate spies" "Unable to find function to replace"
                  ]
              )
    ]
  ]
