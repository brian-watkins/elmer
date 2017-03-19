module Elmer
    exposing
        ( ComponentState
        , PlatformOverride
        , Matcher
        , componentState
        , (<&&>)
        , expectNot
        , each
        , some
        , init
        )

{-| Basic types and functions for working with ComponentStates and Matchers

# Working with ComponentStates
@docs ComponentState, componentState, init

# Working with Matchers
@docs Matcher, (<&&>), expectNot, each, one

# Working with Overrides
@docs PlatformOverride

-}

import Html exposing (Html)
import Native.Platform
import Native.Http
import Native.Html

import Expect
import Elmer.Internal as Internal
import Elmer.Platform as Platform
import Elmer.Runtime as Runtime
import Elmer.Printer exposing (..)

{-| Represents the current state of the component under test.
-}
type alias ComponentState model msg
  = Internal.ComponentState model msg

{-| Represents a request to override a platform function.

See `Elmer.Platform.Command.override` and `Elmer.Platform.Subscription.override` for examples.
-}
type alias PlatformOverride
  = Platform.PlatformOverride

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
componentState model view update =
    Internal.Ready
        { model = model
        , view = view
        , update = update
        , targetSelector = Nothing
        , locationParser = Nothing
        , location = Nothing
        , httpRequests = []
        , deferredCommands = []
        , dummyCommands = []
        , subscriptions = Sub.none
        }

{-| Operator for conjoining matchers.
If one fails, then the conjoined matcher fails, otherwise it passes.

    Elmer.Html.expectElement (
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

{-| Create a matcher that applies the given matcher to each item in a list.

The created matcher will pass if all items satisfy the given matcher.
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

{-| Create a matcher that applies the given matcher to each item in a list.

The created matcher will pass if at least one item satisfies the given matcher.
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


{-| Update the `ComponentState` with the given model and Cmd.

Useful for testing `init` functions.
-}
init : (model, Cmd msg) -> ComponentState model msg -> ComponentState model msg
init ( initModel, initCommand ) =
  Internal.map (\component ->
    let
      updatedComponent = { component | model = initModel }
    in
      case Runtime.performCommand initCommand updatedComponent of
        Ok initializedComponent ->
          Internal.Ready initializedComponent
        Err message ->
          Internal.Failed message
  )
