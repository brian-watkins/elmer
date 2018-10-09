module Elmer.DocumentTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Program
import Elmer.Program.Matchers exposing (expectTitle)
import Elmer.TestState as TestState
import Elmer
import Elmer.Printer exposing (..)
import Elmer.Navigation as Navigation
import Elmer.Html as Markup
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (by)
import Elmer.Errors as Errors
import Elmer.UrlHelpers as UrlHelpers
import Elmer.TestHelpers exposing (expectError)
import Elmer.TestApps.DocumentTestApp as App
import Elmer.TestApps.SimpleTestApp as SimpleApp


all : Test
all =
  Test.concat
  [ givenDocumentTests
  , expectTitleTests
  ]


givenDocumentTests : Test
givenDocumentTests =
  describe "given a document"
  [ test "it creates a TestState" <|
    \() ->
      Elmer.Program.givenDocument App.view App.update
          |> Elmer.Program.init (\() -> App.init ())
          |> Markup.target << by [ Sel.id "some-element" ]
          |> Markup.expect (element <| hasText "Fun Stuff")
  ]


expectTitleTests : Test
expectTitleTests =
  describe "expect application title"
  [ describe "when there is an upstream failure" 
    [ test "it shows the failure" <|
      \() ->
        TestState.failure "Failed!"
          |> expectTitle "Something"
          |> Expect.equal (Expect.fail "Failed!")
    ]
  , describe "when the view function does not result in a document"
    [ test "it fails" <|
      \() ->
        Elmer.given SimpleApp.defaultModel SimpleApp.view SimpleApp.update
          |> expectTitle "Document Title"
          |> expectError (Errors.noTitle "Document Title")
    ]
  , describe "when no model has been set via init"
    [ test "it fails" <|
      \() ->
        Elmer.Program.givenDocument App.view App.update
          |> expectTitle "Wrong Title"
          |> expectError Errors.noModel
    ]
  , describe "when the title is not what is expected"
    [ test "it fails" <|
      \() ->
        Elmer.Program.givenDocument App.view App.update
          |> Elmer.Program.init (\() -> App.init ())
          |> expectTitle "Wrong Title"
          |> expectError (Errors.wrongTitle "Wrong Title" "Fun Title")
    ]
  , describe "when the expected title matches the title"
    [ test "it passes" <|
      \() ->
        Elmer.Program.givenDocument App.view App.update
          |> Elmer.Program.init (\() -> App.init ())
          |> expectTitle "Fun Title"
    ]
  ]
