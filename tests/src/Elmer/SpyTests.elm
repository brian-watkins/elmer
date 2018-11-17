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
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Printer exposing (..)
import Elmer
import Elmer.Program
import Elmer.Http
import Elmer.Http.Matchers as HttpMatchers
import Elmer.Command as Command
import Elmer.Errors as Errors


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
          spy =
            Spy.observe (\_ -> SpyApp.clearName)
              |> Spy.andCallThrough
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
          spy =
            Spy.observe (\_ -> \() -> "huh?")
              |> Spy.andCallThrough
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Expect.equal (TestState.failure <|
              Errors.print (Errors.failedToActivateSpies "Unable to identify a function to spy on")
            )
    ]
  , describe "when the argument references a function"
    [ describe "when the function is called"
      [ test "it still functions normally" <|
        \() ->
          let
            spy =
              Spy.observe (\_ -> SpyApp.clearName)
                |> Spy.andCallThrough
          in
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ spy ]
              |> Markup.target << by [ Sel.id "button" ]
              |> Event.click
              |> Elmer.expectModel (\model ->
                  Expect.equal model.name "Default Name"
                )
      ]
    ]
  ]

testFake : String -> String
testFake word =
  word

expectSpyTests : Test
expectSpyTests =
  describe "expectSpy"
  [ describe "when there is a failure upstream"
    [ test "it fails" <|
      \() ->
        TestState.failure "Upstream Failure"
          |> Spy.expect (\_ -> testFake) (\_ -> Expect.pass)
          |> Expect.equal (Expect.fail "Upstream Failure")
    ]
  , describe "when the function has not been registered as a spy"
    [ test "it fails" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.expect (\_ -> testFake) (\_ -> Expect.pass)
          |> Expect.equal (Errors.failWith <| Errors.unknownSpy "Elmer.SpyTests.testFake")
    ]
  , describe "when a bad identifier is given to Spy.expect"
    [ test "it fails" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.expect (\_ -> (\_ -> "blah")) (\_ -> Expect.pass)
          |> Expect.equal (Errors.failWith <| Errors.badSpyIdentifier)
    ]
  , describe "when the function has been registered as a spy"
    [ describe "when the function has only one argument" <|
      let
        callThroughSpy =
          Spy.observe (\_ -> SpyApp.titleText)
            |> Spy.andCallThrough
      in
      [ test "it sets the name and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ callThroughSpy ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Spy.expect (\_ -> SpyApp.titleText) (\spy ->
                Expect.equal spy.name "Elmer.TestApps.SpyTestApp.titleText"
              )
      , test "it sets the number of calls and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ callThroughSpy ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Markup.render
            |> Spy.expect (\_ -> SpyApp.titleText) (\spy ->
                Expect.equal (List.length spy.calls) 1
              )
      , test "it sets the arguments for each call and passes it to the matcher" <|
        \() ->
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ callThroughSpy ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Markup.render
            |> Spy.expect (\_ -> SpyApp.titleText) (\spy ->
                Expect.equal spy.calls [ [ StringArg "Some Title" ] ]
              )
      , describe "when the spy calls a fake"
        [ test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            let
              spy =
                Spy.observe (\_ -> SpyApp.titleText)
                  |> Spy.andCallFake (\_ -> "Fake Title")
            in
              Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                |> Spy.use [ spy ]
                |> Markup.target << by [ Sel.id "title" ]
                |> Markup.render
                |> Spy.expect (\_ -> SpyApp.titleText) (\actual ->
                    Expect.equal actual.calls [ [ StringArg "Some Title" ] ]
                  )
        , test "it calls the fake" <|
          \() ->
            let
              spy =
                Spy.observe (\_ -> SpyApp.titleText)
                  |> Spy.andCallFake ( \_ -> "Fake Title" )
            in
              Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                |> Spy.use [ spy ]
                |> Markup.target << by [ Sel.id "title" ]
                |> Markup.expect (element <| hasText "Fake Title")
        ]
      ]
    , describe "when the function has multiple arguments"
      [ describe "when all the arguments are provided at once" <|
        let
          combineNamesSpy =
            Spy.observe (\_ -> SpyApp.combineNames)
              |> Spy.andCallThrough
        in
        [ test "it sets the name and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "multi-arg-button" ]
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal spy.name "Elmer.TestApps.SpyTestApp.combineNames"
                )
        , test "it sets the number of calls and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "multi-arg-button" ]
              |> Event.click
              |> Event.click
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal (List.length spy.calls) 3
                )
        , test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "multi-arg-button" ]
              |> Event.click
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal spy.calls [ [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ], [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ] ]
                )
        , describe "when the spy calls a fake"
          [ test "it sets the arguments for each call and passes it to the matcher" <|
            \() ->
              let
                spy =
                  Spy.observe (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Name" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target << by [ Sel.id "multi-arg-button" ]
                  |> Event.click
                  |> Event.click
                  |> Spy.expect (\_ -> SpyApp.combineNames) (\actual ->
                      Expect.equal actual.calls [ [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ], [ StringArg "Dr.", StringArg "Awesome", StringArg "Dude" ] ]
                    )
          , test "it calls the fake" <|
            \() ->
              let
                spy =
                  Spy.observe (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Name" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target << by [ Sel.id "multi-arg-button" ]
                  |> Event.click
                  |> Markup.target << by [ Sel.id "name" ]
                  |> Markup.expect (element <| hasText "Name: Fake Name")
          ]
        ]
      , describe "when arguments are provided successively" <|
        let
          combineNamesSpy =
            Spy.observe (\_ -> SpyApp.combineNames)
              |> Spy.andCallThrough
        in
        [ test "it sets the name and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "successive-arg-button" ]
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal spy.name "Elmer.TestApps.SpyTestApp.combineNames"
                )
        , test "it sets the number of calls and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "successive-arg-button" ]
              |> Event.click
              |> Event.click
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal (List.length spy.calls) 3
                )
        , test "it sets the arguments for each call and passes it to the matcher" <|
          \() ->
            Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
              |> Spy.use [ combineNamesSpy ]
              |> Markup.target << by [ Sel.id "successive-arg-button" ]
              |> Event.click
              |> Event.click
              |> Spy.expect (\_ -> SpyApp.combineNames) (\spy ->
                  Expect.equal spy.calls [ [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ], [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ] ]
                )
        , describe "when the spy calls a fake"
          [ test "it sets the arguments for each call and passes it to the matcher" <|
            \() ->
              let
                spy =
                  Spy.observe (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Stuff" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target << by [ Sel.id "successive-arg-button" ]
                  |> Event.click
                  |> Event.click
                  |> Spy.expect (\_ -> SpyApp.combineNames) (\actual ->
                      Expect.equal actual.calls [ [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ], [ StringArg "Mrs.", StringArg "Funny", StringArg "Animal" ] ]
                    )
          , test "it calls the fake" <|
            \() ->
              let
                spy =
                  Spy.observe (\_ -> SpyApp.combineNames)
                    |> Spy.andCallFake ( \_ _ _ -> "Fake Stuff" )
              in
                Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
                  |> Spy.use [ spy ]
                  |> Markup.target << by [ Sel.id "successive-arg-button" ]
                  |> Event.click
                  |> Markup.target << by [ Sel.id "name" ]
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
          Spy.observe (\_ -> SpyApp.makeModel)
            |> Spy.andCallThrough
      in
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.use [ spy ]
          |> Elmer.Program.init (\_ ->
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
          |> Spy.expect (\_ -> SpyApp.makeModel) (\actual ->
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
          stub = Spy.observe (\_ -> SpyApp.titleText)
            |> Spy.andCallFake (\_ -> "Test Title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Markup.expect (element <| hasText "Test Title")
    , test "it is not active for the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target << by [ Sel.id "title" ]
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  , describe "when a spy is used multiple times in the same test"
    [ test "the spy is set each time" <|
      \() ->
        let
          stub =
            Spy.observe (\_ -> SpyApp.titleText)
              |> Spy.andCallFake (\_ -> "Test Title")
          anotherStub =
            Spy.observe (\_ -> SpyApp.titleText)
              |> Spy.andCallFake (\_ -> "Another test title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Spy.use [ anotherStub ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Markup.expect (element <| hasText "Another test title")
    , test "the spy is not set in the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target << by [ Sel.id "title" ]
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  , describe "when a component state map results in a failure"
    [ test "the spy is set" <|
      \() ->
        let
          stub =
            Spy.observe (\_ -> SpyApp.titleText)
              |> Spy.andCallFake (\_ -> "Test Title")
        in
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ stub ]
            |> Markup.target << by [ Sel.id "title" ]
            |> Event.click
            |> Expect.equal (TestState.failure "No event handlers found for any of the triggered events: click, mousedown, mouseup, submit")
    , test "the spy is not active for the next test" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Markup.target << by [ Sel.id "title" ]
          |> Markup.expect (element <| hasText "A Title: Some Title")
    ]
  ]

andCallFakeTests : Test
andCallFakeTests =
  describe "andCallFake"
  [ describe "when a fake function is specified"
    [ let
        spy =
          Spy.observe (\_ -> SpyApp.titleText)
            |> Spy.andCallFake (\_ -> "Test Title")

        state =
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ spy ]
            |> Markup.target << by [ Sel.id "title" ]
      in
        describe "when the spied on function is called"
          [ test "it calls the fake version" <|
            \() ->
              state
                |> Markup.expect (element <| hasText "Test Title")
          , test "it records the call" <|
            \() ->
              Markup.render state
                |> Spy.expect (\_ -> SpyApp.titleText) (Matchers.wasCalled 1)
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
            |> Markup.target << by [ Sel.id "footer" ]
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
            |> Markup.target << by [ Sel.id "footer" ]
            |> Markup.expect (element <| hasText "I am a fake footer")
            |> Expect.equal (Errors.failWith <| Errors.failedToActivateSpies "Elmer.TestApps.SpyTestApp.titleText is a function, but your test is treating it as a value to be replaced")
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
            |> Markup.target << by [ Sel.id "footer" ]
            |> Markup.expect (element <| hasText "I am a fake footer")
            |> Expect.equal (Errors.failWith <| Errors.failedToActivateSpies "Unable to identify a value to replace")
    ]
  ]
