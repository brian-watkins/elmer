module Elmer
    exposing
        ( componentState
        , navigationComponentState
        , map
        , mapToExpectation
        , (<&&>)
        )

import Html exposing (Html)
import Native.Helpers

import Expect
import Elmer.Html.Node as Node
import Elmer.Types exposing (..)



componentState : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentStateResult model msg
componentState model view update =
    CurrentState
        { model = model
        , view = view
        , update = update
        , targetNode = Nothing
        , locationParser = Nothing
        , location = Nothing
        , httpRequests = []
        , deferredCommands = []
        , dummyCommands = []
        }


navigationComponentState :
    model
    -> ViewFunction model msg
    -> UpdateFunction model msg
    -> LocationParserFunction msg
    -> ComponentStateResult model msg
navigationComponentState model view update parser =
    CurrentState
        { model = model
        , view = view
        , update = update
        , targetNode = Nothing
        , locationParser = Just parser
        , location = Nothing
        , httpRequests = []
        , deferredCommands = []
        , dummyCommands = []
        }

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

map : (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
map mapper componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            mapper componentState

        UpstreamFailure message ->
            UpstreamFailure message

mapToExpectation : (HtmlComponentState model msg -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
mapToExpectation mapper componentStateResult =
  case componentStateResult of
    CurrentState componentState ->
      mapper componentState
    UpstreamFailure message ->
      Expect.fail message
