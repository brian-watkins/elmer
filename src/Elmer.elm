module Elmer
    exposing
        ( TestState
        , Matcher
        , given
        , expectAll
        , expectNot
        , each
        , some
        , exactly
        , atIndex
        , last
        , hasLength
        , expectModel
        )

{-| Basic types and functions for working with tests and matchers

@docs TestState

# Working with Matchers
@docs Matcher, expectAll, expectNot

# List Matchers
@docs each, exactly, some, atIndex, last, hasLength

# Make low-level expectations
@docs given, expectModel

-}

import Html exposing (Html)

import Expect
import Test.Runner
import Elmer.TestState as TestState
import Elmer.Context as Context exposing (View(..))
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)
import Elmer.Errors as Errors exposing (failWith)
import Array

{-| Represents the current state of the test.
-}
type alias TestState model msg
  = TestState.TestState model msg

{-| Generic type for functions that pass or fail.

A matcher returns an `Expect.Expectation` from the
[elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/latest)
package.
-}
type alias Matcher a =
  (a -> Expect.Expectation)

{-| Initialize a test with a model, view function, and update function.
-}
given : 
  model
    -> ( model -> Html msg )
    -> ( msg -> model -> ( model, Cmd msg ) )
    -> TestState model msg
given model view update =
  Context.default (HtmlView view) update
    |> Context.withModel model
    |> TestState.with


{-| Expect that all matchers pass.
If one fails, then the conjoined matcher fails, otherwise it passes.

    Elmer.given someModel view update
      |> Elmer.Html.expect (
        Elmer.Html.Matchers.element <|
          Elmer.Html.expectAll
            [ Elmer.Html.Matchers.hasText "Awesome"
            , Elmer.Html.Matchers.hasClass "cool"
            ]
        )
-}
expectAll : List (Matcher a) -> Matcher a
expectAll matchers =
  \value ->
    List.foldl (\matcher result ->
      case Test.Runner.getFailureReason result of
        Just _ ->
          result
        Nothing ->
          matcher value
    ) Expect.pass matchers


{-| Make expectations about the model in its current state.

    Elmer.given defaultModel view update
      |> Elmer.Html.target "button"
      |> Elmer.Html.Event.click
      |> Elmer.expectModel (\model ->
        Expect.equal model.clickCount 1
      )

Use Elmer to get the model into a certain state. Then use the normal facilities of
elm-test to describe how the model should look in that state.
-}
expectModel : Matcher model -> Matcher (TestState model msg)
expectModel matcher =
  TestState.mapToExpectation <|
    \context ->
      case Context.model context of
        Just model ->
          matcher model
        Nothing ->
          failWith Errors.noModel


{-| Expect that a matcher fails.
-}
expectNot : Matcher a -> Matcher a
expectNot matcher =
  \node ->
    let
      result = matcher node
    in
      if result == Expect.pass then
        Expect.fail "Expected not to be the case but it is"
      else
        Expect.pass


{-| Expect that all items in a list satisfy the given matcher.
-}
each : Matcher a -> Matcher (List a)
each matcher =
  \list ->
    let
      failures = takeFailures matcher list
    in
      if List.isEmpty list then
        Expect.fail <| format
          [ description "Expected all to pass but the list is empty" ]
      else
        if List.isEmpty failures then
          Expect.pass
        else
          Expect.fail <| format
            [ description "Expected all to pass but some failed:"
            , description <| formatFailures failures
            ]

{-| Expect that at least one item in a list satisfies the given matcher.
-}
some : Matcher a -> Matcher (List a)
some matcher =
  \list ->
    let
      failures = takeFailures matcher list
    in
      if List.isEmpty list then
        Expect.fail <| format
          [ description "Expected some to pass but the list is empty" ]
      else
        if List.length failures < List.length list then
          Expect.pass
        else
          Expect.fail <| format
            [ description "Expected some to pass but found none. Here are the failures:"
            , description <| formatFailures failures
            ]

{-| Expect that exactly some number of items in a list satisfy the given matcher.
-}
exactly : Int -> Matcher a -> Matcher (List a)
exactly expectedCount matcher =
  \list ->
    let
      failures = takeFailures matcher list
      matchCount = List.length list - List.length failures
    in
      if matchCount == expectedCount then
        Expect.pass
      else
        if List.isEmpty failures then
          Expect.fail <| format
            [ description <| "Expected exactly " ++ (String.fromInt expectedCount) ++
              " to pass but the list is empty"
            ]
        else
          Expect.fail <| format
            [ description <| "Expected exactly " ++ (String.fromInt expectedCount) ++
              " to pass but found " ++ (String.fromInt matchCount) ++ ". Here are the failures:"
            , description <| formatFailures failures
            ]

{-| Expect that the item at the given index satisfies the given matcher.
-}
atIndex : Int -> Matcher a -> Matcher (List a)
atIndex index matcher =
  \list ->
    case Array.fromList list |> Array.get index of
      Just item ->
        case Test.Runner.getFailureReason <| matcher item of
          Just failure ->
            Expect.fail <| format
              [ description <| "Expected item at index " ++ (String.fromInt index) ++ " to pass but it failed:"
              , description <| formatFailure failure
              ]
          Nothing ->
            Expect.pass
      Nothing ->
        Expect.fail <| format
          [ description <| "Expected item at index " ++ (String.fromInt index) ++ " to pass but there is no item at that index"
          ]

takeFailures : Matcher a -> List a -> List FailureReason
takeFailures matcher =
  List.filterMap (\item ->
    Test.Runner.getFailureReason <| matcher item
  )

{-| Expect that the last item in a list satisfies the given matcher.
-}
last : Matcher a -> Matcher (List a)
last matcher =
  \list ->
    let
      maybeItem =
        List.drop ((List.length list) - 1) list
          |> List.head
    in
      case maybeItem of
        Just item ->
          case Test.Runner.getFailureReason <| matcher item of
            Just failure ->
              Expect.fail <| format
                [ description <| "Expected the last item to pass but it failed:"
                , description <| formatFailure failure
                ]
            Nothing ->
              Expect.pass
        Nothing ->
          Expect.fail <| format
            [ description <| "Expected the last item to pass but the list is empty"
            ]


{-| Expect that a list has the given length.
-}
hasLength : Int -> Matcher (List a)
hasLength expectedCount =
  \list ->
    if List.length list == expectedCount then
      Expect.pass
    else
      Expect.fail <|
        format
          [ message "Expected list to have size" (String.fromInt expectedCount)
          , message "but it has size" (String.fromInt (List.length list))
          ]
