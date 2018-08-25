module Elmer.Application exposing
  ( given
  )

{-| Functions for testing Elm HTML applications

# Test an Application
@docs given

-}

import Elmer exposing (TestState)
import Elmer.Context as Context
import Elmer.TestState as TestState
import Browser exposing (UrlRequest, Document)
import Url exposing (Url)
import Html exposing (Html)


{-| Initialize a `TestState` with the basic requirements for an Elm Html application: taggers for responding to new url requests
and url changes, a view function that results in a `Document msg`, and an update function. 

Use this function to initialize tests for Html applications created with `Browser.application`.
-}
given : (UrlRequest -> msg) -> (Url -> msg) -> (model -> Document msg) -> (msg -> model -> (model, Cmd msg)) -> TestState model msg
given onUrlRequest onUrlChange view update =
  Context.default (documentView view) update
    |> TestState.with


documentView : (model -> Document msg) -> model -> Html msg
documentView viewForDocument model =
  viewForDocument model
    |> .body
    |> Html.node "body" []