module Elmer
    exposing
        ( componentState
        , navigationComponentState
        , find
        , expectNode
        , expectNodeExists
        , map
        , failureCommand
        )

import Html exposing (Html)
import Native.Helpers

import Expect
import Task
import Elmer.Node as Node
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
        }


map : (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
map mapper componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            mapper componentState

        UpstreamFailure message ->
            UpstreamFailure message


expectNode : (HtmlNode msg -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectNode expectFunction componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            case componentState.targetNode of
                Just node ->
                    expectFunction node

                Nothing ->
                    Expect.fail "Node does not exist"

        UpstreamFailure message ->
            Expect.fail message


expectNodeExists : ComponentStateResult model msg -> Expect.Expectation
expectNodeExists componentStateResult =
    expectNode (\_ -> Expect.pass) componentStateResult


find : String -> ComponentStateResult model msg -> ComponentStateResult model msg
find selector =
    map (updateTargetNode selector)


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



type alias TaskFailure =
  { elmerError: String
  }

failureCommand : String -> Cmd msg
failureCommand message =
  Task.attempt errorTagger (Task.fail (taskFailure message))

errorTagger : Result TaskFailure a -> msg
errorTagger result =
  case result of
    Ok err ->
      Debug.crash "Died"
    Err _ ->
      Debug.crash "Died"

taskFailure : String -> TaskFailure
taskFailure msg =
  { elmerError = msg }
