module Elmer exposing (HtmlNode, HtmlComponentState, HtmlElementList(..), HtmlElement(..), ComponentStateResult(..), hasClass, expectNode, initialComponentState, resolve, find, findResult, hasText, click, clickResult, inputResult, doUpdate, SearchResult(..), EventResult(..), UpdateResult(..))

import Html exposing (Html)
import Native.Helpers
import Json.Decode as Json exposing (..)
import Result
import Json.Decode.Extra as JsonExtra
import String
import Expect

type HtmlElement =
  Node HtmlNode |
  Text String

type alias HtmlNode =
  { tag: String
  , id: Maybe String
  , classes: Maybe ( List String )
  , children: HtmlElementList
  , events: Maybe ( HtmlEvents )
  }

type HtmlElementList =
  HtmlElementList (List HtmlElement)

type alias HtmlEvents =
  { click: Maybe HtmlEvent
  , input: Maybe HtmlEvent
  }

type alias HtmlEvent =
  { eventType: String
  , decoder: RawValue
  }

type RawValue = RawValue

type SearchResult =
  Found HtmlNode |
  SearchFailure String

type EventResult msg =
  Message msg |
  EventFailure String

type UpdateResult model msg =
  UpdateEffect ( model, Cmd msg ) |
  UpdateFailure String

type alias ViewFunction model msg =
  model -> Html msg

type alias UpdateFunction model msg =
  msg -> model -> ( model, Cmd msg )

type alias HtmlComponentState model msg =
  { model: model
  , view: ViewFunction model msg
  , update: UpdateFunction model msg
  , targetNode : Maybe HtmlNode
  }

initialComponentState : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentStateResult model msg
initialComponentState model view update =
  CurrentState { model = model
  , view = view
  , update = update
  , targetNode = Nothing
  }

type ComponentStateResult model msg =
  CurrentState (HtmlComponentState model msg) |
  UpstreamFailure String





inputResult : String -> SearchResult -> EventResult msg
inputResult inputString searchResult =
  passNodeOrFail searchResult (inputCaller inputString)

inputCaller : String -> HtmlNode -> EventResult msg
inputCaller inputString node =
  case node.events `Maybe.andThen` .input of
    Just inputEvent ->
      let
        eventJson = "{\"target\":{\"value\":\"" ++ inputString ++ "\"}}"
      in
        handleEventResult (Native.Helpers.getMessageForEvent inputEvent.decoder eventJson)
    Nothing ->
      EventFailure "No input event found"

clickResult : SearchResult -> EventResult msg
clickResult searchResult =
  passNodeOrFail searchResult clickCaller

clickCaller : HtmlNode -> EventResult msg
clickCaller node =
  case node.events `Maybe.andThen` .click of
    Just clickEvent ->
      handleEventResult (Native.Helpers.getMessageForEvent clickEvent.decoder "{}")
    Nothing ->
      EventFailure "No click event found"

handleEventResult : Result String msg -> EventResult msg
handleEventResult eventResult =
  case eventResult of
    Ok m ->
      Message m
    Err e ->
      EventFailure e

passNodeOrFail : SearchResult -> (HtmlNode -> EventResult msg) -> EventResult msg
passNodeOrFail searchResult eventCaller =
  case searchResult of
    Found node ->
      eventCaller node
    SearchFailure msg ->
      EventFailure msg

doUpdate : (UpdateFunction model msg) -> model -> EventResult msg -> UpdateResult model msg
doUpdate updateFunction model eventResult =
  case eventResult of
    Message m ->
      UpdateEffect (updateFunction m model)
    EventFailure e ->
      UpdateFailure e

resolve : (model -> Expect.Expectation) -> UpdateResult model msg -> Expect.Expectation
resolve expectFunction updateResult =
  case updateResult of
    UpdateEffect (model, _) ->
      expectFunction model
    UpdateFailure msg ->
      Expect.fail msg



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

click : ComponentStateResult model msg -> ComponentStateResult model msg
click componentStateResult =
  componentStateOrFail componentStateResult (
    \componentState ->
      case componentState.targetNode of
        Just node ->
          case clickResult (Found node) of
            Message msg ->
              let
                (updatedModel, command) = componentState.update msg componentState.model
              in
                CurrentState { componentState | model = updatedModel }
            EventFailure msg ->
              UpstreamFailure msg
        Nothing ->
          UpstreamFailure "No target node specified"
  )

componentStateOrFail : ComponentStateResult model msg -> (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg
componentStateOrFail componentStateResult stateFunction =
  case componentStateResult of
    CurrentState componentState ->
      stateFunction componentState
    UpstreamFailure message ->
      UpstreamFailure message





findResult : Html msg -> String -> SearchResult
findResult html selector =
  case Native.Helpers.findHtmlNode selector html of
    Just node ->
      Found node
    Nothing ->
      SearchFailure ("No html node found with selector: " ++ selector)

hasText : String -> HtmlNode -> Expect.Expectation
hasText text node =
  let
    texts = List.filterMap extractText (unwrapElementList node.children)
  in
    if List.length texts == 0 then
      Expect.fail ("Expected node to have text '" ++ text ++ "' but it has no text")
    else
      if List.member text texts then
        Expect.pass
      else
        Expect.fail ("Expected node to have text '" ++ text ++ "' but it has text: " ++ (printList texts))


printList : List String -> String
printList list =
  String.join ", " list

hasClass : String -> HtmlNode -> Bool
hasClass className node =
  case node.classes of
    Just classList ->
      List.member className classList
    Nothing ->
      False

extractText : HtmlElement -> Maybe String
extractText element =
  case element of
    Node _ ->
      Nothing
    Text text ->
      Just text

unwrapElementList : HtmlElementList -> List HtmlElement
unwrapElementList (HtmlElementList nodeList) =
  nodeList
