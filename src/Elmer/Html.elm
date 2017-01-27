module Elmer.Html exposing
  ( find
  , expectNode
  , expectNodeExists
  )

import Elmer
import Elmer.Types exposing (..)
import Html exposing (Html)
import Elmer.Html.Node as Node
import Expect

expectNode : (HtmlNode msg -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectNode expectFunction =
  Elmer.mapToExpectation <|
    \componentState ->
      case componentState.targetNode of
        Just node ->
          expectFunction node
        Nothing ->
          Expect.fail "Node does not exist"


expectNodeExists : ComponentStateResult model msg -> Expect.Expectation
expectNodeExists componentStateResult =
    expectNode (\_ -> Expect.pass) componentStateResult


find : String -> ComponentStateResult model msg -> ComponentStateResult model msg
find selector =
    Elmer.map (updateTargetNode selector)


updateTargetNode : String -> HtmlComponentState model msg -> ComponentStateResult model msg
updateTargetNode selector componentState =
  let
    currentView = componentState.view componentState.model
  in
    case Node.findNode currentView selector of
        Just node ->
            CurrentState { componentState | targetNode = Just node }

        Nothing ->
          let
            failure = "No html node found with selector: " ++ selector ++ "\n\nThe current view is:\n\n"
              ++ (htmlToString currentView)
          in
            UpstreamFailure failure


htmlToString : Html msg -> String
htmlToString htmlMsg =
  case Native.Helpers.asHtmlNode htmlMsg of
    Just node ->
      Node.toString node
    Nothing ->
      "<No Nodes>"
