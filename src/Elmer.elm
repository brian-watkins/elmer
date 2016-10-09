module Elmer exposing (componentState, find, findResult, expectNode)

import Html exposing (Html)
import Native.Helpers
import Json.Decode as Json exposing (..)
import Result
import Json.Decode.Extra as JsonExtra
import String
import Expect
import Task exposing (Task)

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
      case findResult (componentState.view componentState.model) selector of
        Found n ->
          CurrentState { componentState | targetNode = Just n }
        SearchFailure message ->
          UpstreamFailure message
  )

findResult : Html msg -> String -> SearchResult
findResult html selector =
  case Native.Helpers.findHtmlNode selector html of
    Just node ->
      Found node
    Nothing ->
      SearchFailure ("No html node found with selector: " ++ selector)
