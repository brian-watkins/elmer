module Elmer
    exposing
        ( ComponentState
        , Matcher
        , componentState
        , (<&&>)
        , expectNot
        , each
        , some
        , exactly
        , hasLength
        , init
        , expectModel
        )

{-| Basic types and functions for working with ComponentStates and Matchers

# Working with ComponentStates
@docs ComponentState, componentState, init, expectModel

# Working with Matchers
@docs Matcher, (<&&>), expectNot

# List Matchers
@docs each, exactly, some, hasLength

-}

import Html exposing (Html)
import Native.Platform
import Native.Http
import Native.Html
import Native.Spy

import Expect
import Elmer.ComponentState as ComponentState
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)

{-| Represents the current state of the component under test.
-}
type alias ComponentState model msg
  = ComponentState.ComponentState model msg

{-| Generic type for functions that pass or fail.

A matcher returns an `Expect.Expectation` from the
[elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/latest)
package.
-}
type alias Matcher a =
  (a -> Expect.Expectation)

{-| Basic constructor for a `ComponentState`.
-}
componentState
  :  model
  -> ( model -> Html msg )
  -> ( msg -> model -> ( model, Cmd msg ) )
  -> ComponentState model msg
componentState =
  ComponentState.create

{-| Operator for conjoining matchers.
If one fails, then the conjoined matcher fails, otherwise it passes.

    Elmer.Html.expect (
      Elmer.Html.Matchers.element <|
        Elmer.Html.Matchers.hasText "Awesome" <&&>
        Elmer.Html.Matchers.hasClass "cool"
    ) componentState
-}
(<&&>) : Matcher a -> Matcher a -> Matcher a
(<&&>) leftFunction rightFunction =
  (\node ->
    let
      leftResult = leftFunction node
      rightResult = rightFunction node
    in
      if leftResult == Expect.pass then
        rightResult
      else
        leftResult
  )

{-| Make expectations about the model in its current state.

    Elmer.componentState defaultModel view update
      |> Elmer.Html.target "button"
      |> Elmer.Html.Event.click
      |> Elmer.expectModel (\model ->
        Expect.equal model.clickCount 1
      )

Use Elmer to get the model into a certain state. Then use the normal facilities of
elm-test to describe how the model should look in that state.
-}
expectModel : Matcher model -> Matcher (ComponentState model msg)
expectModel matcher =
  ComponentState.mapToExpectation (\component ->
    matcher component.model
  )


{-| Expect that a matcher fails.
-}
expectNot : Matcher a -> Matcher a
expectNot matcher =
  (\node ->
    let
      result = matcher node
    in
      if result == Expect.pass then
        Expect.fail "Expected not to be the case but it is"
      else
        Expect.pass
  )

{-| Expect that all items in a list satisfy the given matcher.
-}
each : Matcher a -> Matcher (List a)
each matcher list =
  case Expect.getFailure <| expectAll matcher list of
    Just failure ->
      Expect.fail <| format [ message "An item failed to match" failure.message ]
    Nothing ->
      Expect.pass

expectAll : Matcher a -> Matcher (List a)
expectAll matcher list =
  case list of
    [] ->
      Expect.pass
    x :: xs ->
      List.foldl (\item r ->
        if r == Expect.pass then
          matcher item
        else
          r
      ) (matcher x) xs

{-| Expect that at least one item in a list satisfies the given matcher.
-}
some : Matcher a -> Matcher (List a)
some matcher list =
  case Expect.getFailure <| expectAny matcher list of
    Just _ ->
      Expect.fail "No items matched"
    Nothing ->
      Expect.pass

expectAny : Matcher a -> Matcher (List a)
expectAny matcher =
  List.foldl (\item testResult ->
    case Expect.getFailure testResult of
      Just _ ->
        matcher item
      Nothing ->
        testResult
  ) (Expect.fail "Nothing")

{-| Expect that exactly some number of items in a list satisfy the given matcher.
-}
exactly : Int -> Matcher a -> Matcher (List a)
exactly expectedCount matcher list =
  let
    matchCount = countMatches matcher list
  in
    if matchCount == expectedCount then
      Expect.pass
    else
      Expect.fail <| format
        [ message "Expected number of matches" (toString expectedCount)
        , message "But the actual number of matches is" (toString matchCount)
        ]

countMatches : Matcher a -> List a -> Int
countMatches matcher list =
  List.foldl (\item times ->
    if (matcher item) == Expect.pass then
      times + 1
    else
      times
  ) 0 list

{-| Expect that a list has the given length.
-}
hasLength : Int -> Matcher (List a)
hasLength expectedCount list =
  if List.length list == expectedCount then
    Expect.pass
  else
    Expect.fail <|
      format
        [ message "Expected list to have size" (toString expectedCount)
        , message "but it has size" (toString (List.length list))
        ]

{-| Update the `ComponentState` with the given model and Cmd.

The current model will be replaced by the given model and the given command
will then be executed. This is most useful for testing `init` functions.

The first argument takes a wrapper around whatever function produces the initial
model and command. This allows Elmer to evaluate the initializing function lazily,
in case any stubs need to be applied.

    Elmer.componentState MyComponent.defaultModel MyComponent.view MyComponent.update
      |> init (\() -> MyComponent.init)
      |> Elmer.Html.target "#title"
      |> Elmer.Html.expectElementExists

-}
init : (() -> (model, Cmd msg)) -> ComponentState model msg -> ComponentState model msg
init initThunk =
  ComponentState.map (\component ->
    let
      (initModel, initCommand) = initThunk ()
      updatedComponent = { component | model = initModel }
    in
      case Runtime.performCommand initCommand updatedComponent of
        Ok initializedComponent ->
          ComponentState.with initializedComponent
        Err message ->
          ComponentState.failure message
  )
