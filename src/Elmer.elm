module Elmer exposing (componentState, find, findNode, expectNode)

import Html exposing (Html)
import Native.Helpers
import Expect

import Elmer.Shared exposing (..)
import Elmer.Types exposing(..)

componentState : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentStateResult model msg
componentState model view update =
  CurrentState { model = model
  , view = view
  , update = update
  , targetNode = Nothing
  }

expectNode : (HtmlNode -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectNode expectFunction componentStateResult =
  case componentStateResult of
    CurrentState componentState ->
      case componentState.targetNode of
        Just node ->
          expectFunction node
        Nothing ->
          Expect.fail "No target node specified"
    UpstreamFailure message ->
      Expect.fail message

find : String -> ComponentStateResult model msg -> ComponentStateResult model msg
find selector componentStateResult =
  componentStateOrFail componentStateResult (
    \componentState ->
      case findNode (componentState.view componentState.model) selector of
        Just node ->
          CurrentState { componentState | targetNode = Just node }
        Nothing ->
          UpstreamFailure ("No html node found with selector: " ++ selector)
  )

findNode : Html msg -> String -> Maybe HtmlNode
findNode html selector =
  Native.Helpers.findHtmlNode selector html
