module Elmer
    exposing
        ( ComponentState
        , PlatformOverride
        , Matcher
        , componentState
        , (<&&>)
        , expectNot
        )

{-| Basic types and functions for working with ComponentStates and Matchers

# Working with ComponentStates
@docs ComponentState, componentState

# Working with Matchers
@docs Matcher, (<&&>), expectNot

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
        , targetElement = Nothing
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
