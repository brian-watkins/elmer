module Elmer.SpyMatcherTests exposing (..)

import Test exposing (..)
import Test.Runner
import Expect
import Elmer.TestApps.SpyTestApp as SpyApp
import Elmer.Spy as Spy exposing (Calls)
import Elmer.Spy.Matchers as Matchers exposing (stringArg, argThat)
import Elmer.Spy.Arg exposing (Arg(..))
import Elmer.Html as Markup
import Elmer.Html.Event as Event
import Elmer.Html.Matchers exposing (hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Printer exposing (..)
import Elmer


all : Test
all =
  Test.concat
  [ wasCalledTests
  , wasCalledWithTests
  , argMatcherTests
  , anyArgumentTests
  , callsTests
  , hasArgsTests
  , argThatTests
  ]

type FunType
  = Fruit String
  | Bird String

type alias FunStuff =
  { name : String }

funStuff : String -> FunStuff
funStuff funThing =
  { name = funThing
  }


wasCalledTests : Test
wasCalledTests =
  describe "wasCalled"
  [ describe "when the spy has not been called"
    [ test "it fails with the message" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.use [ Spy.create "clearName" (\_ -> SpyApp.clearName) ]
          |> Spy.expect "clearName" (Matchers.wasCalled 2)
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy clearName to have been called" "2 times"
              , message "but it was called" "0 times"
              ]
            )
    , test "it fails with a properly depluralized message" <|
      \() ->
        Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
          |> Spy.use [ Spy.create "clearName" (\_ -> SpyApp.clearName) ]
          |> Spy.expect "clearName" (Matchers.wasCalled 1)
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
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ Spy.create "clearName" (\_ -> SpyApp.clearName) ]
            |> Markup.target << by [ id "button" ]
            |> Event.click
            |> Event.click
            |> Spy.expect "clearName" (Matchers.wasCalled 3)
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
          Elmer.given SpyApp.defaultModel SpyApp.view SpyApp.update
            |> Spy.use [ Spy.create "clearName" (\_ -> SpyApp.clearName) ]
            |> Markup.target << by [ id "button" ]
            |> Event.click
            |> Event.click
            |> Spy.expect "clearName" (Matchers.wasCalled 2)
            |> Expect.equal (Expect.pass)
      ]
    ]
  ]

testCalls : String -> List (List Arg) -> Calls
testCalls name args =
  { name = name
  , calls = args
  }

wasCalledWithTests : Test
wasCalledWithTests =
  describe "wasCalledWith"
  [ describe "when the spy has not been called"
    [ test "it fails" <|
      \() ->
        Matchers.wasCalledWith [ stringArg "blah", stringArg "fun", stringArg "sun" ] (testCalls "test-spy" [])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy test-spy to have been called with" "[ \"blah\"\n, \"fun\"\n, \"sun\"\n]"
              , description "but it was not called"
              ]
            )
    ]
  , describe "when the spy has been called with other args"
    [ test "it fails" <|
      \() ->
        Matchers.wasCalledWith [ stringArg "blah" ] (testCalls "test-spy" [ [ StringArg "not blah" ], [ StringArg "something" ] ])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy test-spy to have been called with" "[ \"blah\"\n]"
              , message "but it was called with" "[ \"not blah\"\n]\n\n[ \"something\"\n]"
              ]
            )
    ]
  , describe "when a call matches"
    [ test "it passes" <|
      \() ->
        Matchers.wasCalledWith [ stringArg "blah" ] (testCalls "test-spy" [ [ StringArg "not blah" ], [ StringArg "blah" ] ])
          |> Expect.equal Expect.pass
    ]
  , describe "when the any argument matcher is used"
    [ test "it passes" <|
      \() ->
        Matchers.wasCalledWith [ Matchers.anyArg ] (testCalls "test-spy" [ [ StringArg "not blah" ], [ StringArg "blah" ] ] )
          |> Expect.equal Expect.pass
    ]
  ]

argMatcherTests : Test
argMatcherTests =
  describe "Argument Matcher Tests"
  [ argumentTests "Int" (Matchers.intArg 23) "23" (IntArg 23)
  , argumentTests "Float" (Matchers.floatArg 23.54) "23.54" (FloatArg 23.54)
  , argumentTests "Bool" (Matchers.boolArg True) "true" (BoolArg True)
  , argumentTests "Typed Record" (Matchers.typedArg <| funStuff "Beach") "{ name = \"Beach\" }" (TypedArg <| cast { name = "Beach" })
  , argumentTests "Typed Union" (Matchers.typedArg <| Bird "Owl") "Bird \"Owl\"" (TypedArg <| (cast Bird "Owl"))
  , argumentTests "Typed List" (Matchers.typedArg <| [ "Fun", "Sun", "Beach" ]) "[\"Fun\",\"Sun\",\"Beach\"]" (TypedArg <| cast ["Fun","Sun","Beach"])
  , argumentTests "Function" (Matchers.functionArg) "<FUNCTION>" FunctionArg
  ]

argumentTests : String -> Arg -> String -> Arg -> Test
argumentTests name expected output actual =
  describe ( name ++ " arg" )
  [ describe "when the spy has been called with other args"
    [ test "it fails" <|
      \() ->
        Matchers.wasCalledWith [ expected ] (testCalls "test-spy" [ [ StringArg "blah" ] ])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy test-spy to have been called with" <| "[ " ++ output ++ "\n]"
              , message "but it was called with" "[ \"blah\"\n]"
              ])
    ]
  , describe "when a call matches"
    [ test "it passes" <|
      \() ->
        Matchers.wasCalledWith [ expected ] (testCalls "test-spy" [ [ actual ] ])
          |> Expect.equal Expect.pass
    ]
  , describe "when the anyArg matcher is used"
    [ test "it passes" <|
      \() ->
        Matchers.wasCalledWith [ Matchers.anyArg ] (testCalls "test-spy" [ [ actual ] ])
          |> Expect.equal Expect.pass
    ]
  ]

anyArgumentTests : Test
anyArgumentTests =
  describe "Any arg"
  [ describe "when the match fails and an any argument is used"
    [ test "it prints the proper message" <|
      \() ->
        Matchers.wasCalledWith [ stringArg "huh", Matchers.anyArg ] (testCalls "test-spy" [ [ StringArg "blah", StringArg "something" ] ])
          |> Expect.equal (Expect.fail <|
            format
              [ message "Expected spy test-spy to have been called with" <| "[ \"huh\"\n, <ANY>\n]"
              , message "but it was called with" "[ \"blah\"\n, \"something\"\n]"
              ])
    ]
  ]

callsTests : Test
callsTests =
  describe "calls"
  [ describe "when the list matcher passes"
    [ test "it makes the calls available to the matcher" <|
      \() ->
        let
          sampleCalls = testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
        in
          sampleCalls
            |> Matchers.calls (Elmer.hasLength 2)
            |> Expect.equal (Expect.pass)
    ]
  , describe "when the list matcher fails"
    [ test "it prints an error message with the spy name" <|
      \() ->
        let
          sampleCalls = testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
        in
          sampleCalls
            |> Matchers.calls (Elmer.hasLength 14)
            |> Expect.equal (Expect.fail <|
              format
                [ description "Expectation for test-spy failed."
                , message "Expected list to have size" "14"
                , message "but it has size" "2"
                ])
    ]
  ]

hasArgsTests : Test
hasArgsTests =
  describe "hasArgs"
  [ describe "when the call has the args"
    [ test "it matches " <|
      \() ->
        let
          sampleCalls = testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
        in
          sampleCalls
            |> Matchers.calls (Elmer.exactly 1 <| Matchers.hasArgs [ StringArg "what" ])
            |> Expect.equal (Expect.pass)
    ]
  , describe "when the call does not have the args"
    [ test "it fails with the message" <|
      \() ->
        let
          sampleCalls = testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
        in
          sampleCalls
            |> Matchers.calls (Elmer.exactly 1 <| Matchers.hasArgs [ StringArg "something else" ])
            |> Expect.equal (Expect.fail <|
              format
                [ description "Expectation for test-spy failed."
                , description <| format
                  [ description "Expected exactly 1 to pass but found 0. Here are the failures:"
                  , description <| format
                    [ message "Expected spy to have been called with" "[ \"something else\"\n]"
                    , message "but it was called with" "[ \"blah\"\n]"
                    , message "Expected spy to have been called with" "[ \"something else\"\n]"
                    , message "but it was called with" "[ \"what\"\n]"
                    ]
                  ]
                ])
    ]
  ]

argThatTests : Test
argThatTests =
  describe "argThat"
  [ argThatBehavior "wasCalledWith" Matchers.wasCalledWith
  , argThatBehavior "hasArgs" hasArgsMatcher
  ]

hasArgsMatcher : List Arg -> Elmer.Matcher Calls
hasArgsMatcher expectedArgs =
  Matchers.calls <| Elmer.some <| Matchers.hasArgs expectedArgs

argThatBehavior : String -> (List Arg -> Elmer.Matcher Calls) -> Test
argThatBehavior name matcher =
  describe ("Using " ++ name)
  [ describe "when the argument satisfies the function"
    [ test "it matches" <|
      \() ->
        testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
          |> matcher [ argThat <| Expect.equal "what" ]
          |> Expect.equal (Expect.pass)
    , test "it matches a typed argument" <|
      \() ->
        testCalls "test-spy" [ [ TypedArg <| cast { name = "Cool Dude", age = 74 } ] ]
          |> matcher
            [ argThat <|
                \record ->
                  record.age
                    |> Expect.equal 74
            ]
          |> Expect.equal (Expect.pass)
    ]
  , describe "when the argument does not satisfy the function"
    [ test "it fails" <|
      \() ->
        testCalls "test-spy" [ [ StringArg "blah" ], [ StringArg "what"] ]
          |> matcher [ argThat <| Expect.equal "something else" ]
          |> Elmer.expectAll
            [ expectFailureContains "\"something else\" is not equal to \"blah\""
            , expectFailureContains "\"something else\" is not equal to \"what\""
            ]
    ]
  , describe "when argThat is used to match a function argument"
    [ test "it fails" <|
      \() ->
        testCalls "test-spy" [ [ FunctionArg ] ]
          |> matcher [ argThat <| Expect.equal "some string" ]
          |> expectFailureContains (format
              [ message "to have been called with" <| "[ <ARG_THAT>\n]"
              , message "but it was called with" "[ <FUNCTION>\n]"
              , description "An argThat matcher failed:"
              , description <| format
                [ description "argThat cannot be used to match arguments that are functions"
                ]
              ]
          )
    ]
  ]

expectFailureContains : String -> Expect.Expectation -> Expect.Expectation
expectFailureContains expected expectation =
  case Test.Runner.getFailureReason expectation of
    Just failure ->
      if String.contains expected failure.description then
        Expect.pass
      else
        Expect.fail <|
          "'" ++ failure.description ++ "'\n\ndoes not contain\n\n'" ++ expected ++ "'"
    Nothing ->
      Expect.fail <|
        "Expected expectation to fail but it passed"


cast : a -> b
cast =
  Elm.Kernel.Value.cast