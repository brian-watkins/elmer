module Elmer.Document exposing
  ( given
  )

{-| Functions for testing Elm Html Documents
  REVISIT

# Test a Document
@docs given

-}

import Expect
import Browser exposing (Document)
import Elmer exposing (Matcher, TestState)
import Elmer.TestState as TestState
import Elmer.Context as Context exposing (View(..))
import Html exposing (Html)


{-| Initialize a `TestState` with the basic requirements for an Elm Document: a view function that results
in a Document value and an update function

Use this function to initialize tests for Html applications created with `Browser.document`.
-}
given : (model -> Document msg) -> (msg -> model -> (model, Cmd msg)) -> TestState model msg
given view update =
  Context.default (DocumentView view) update
    |> TestState.with
