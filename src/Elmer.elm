module Elmer exposing
  ( componentState
  , find
  , findNode
  , expectNode
  , expectNodeExists
  , map
  , not
  , HtmlElement(..)
  , HtmlNode
  , HtmlEvent
  , HtmlComponentState
  , ComponentStateResult(..)
  )

import Html exposing (Html)
import Native.Helpers
import Expect

type HtmlElement =
  Node HtmlNode |
  Text String

type alias HtmlNode =
  { tag: String
  , id: Maybe String
  , classes: Maybe ( List String )
  , children: List HtmlElement
  , events: List HtmlEvent
  }

type alias HtmlEvent =
  { eventType: String
  , decoder: RawValue
  }

type RawValue = RawValue

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

type ComponentStateResult model msg =
  CurrentState (HtmlComponentState model msg) |
  UpstreamFailure String


componentState : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentStateResult model msg
componentState model view update =
  CurrentState { model = model
  , view = view
  , update = update
  , targetNode = Nothing
  }

map : (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
map mapper componentStateResult =
  case componentStateResult of
    CurrentState componentState ->
      mapper componentState
    UpstreamFailure message ->
      UpstreamFailure message

not : Expect.Expectation -> Expect.Expectation
not expectation =
  if expectation == Expect.pass then
    Expect.fail "Expected to fail, but it passed"
  else
    Expect.pass

expectNode : (HtmlNode -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
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
find selector componentStateResult =
  componentStateResult
    |> map (updateTargetNode selector)

updateTargetNode : String -> HtmlComponentState model msg -> ComponentStateResult model msg
updateTargetNode selector componentState =
  case findNode (componentState.view componentState.model) selector of
    Just node ->
      CurrentState { componentState | targetNode = Just node }
    Nothing ->
      UpstreamFailure ("No html node found with selector: " ++ selector)

findNode : Html msg -> String -> Maybe HtmlNode
findNode html selector =
  case Native.Helpers.asHtmlNode html of
    Just node ->
      findWithinNode selector node
    Nothing ->
      Nothing

findWithinNode : String -> HtmlNode -> Maybe HtmlNode
findWithinNode selector root =
  if matchesNode selector root then
    Just root
  else
    findFirstInList (\n -> isSomething (findWithinNode selector n)) (takeNodes root.children)

matchesNode : String -> HtmlNode -> Bool
matchesNode selector node =
  (matchesId selector node) || (matchesClass selector node)

matchesId : String -> HtmlNode -> Bool
matchesId selector node =
  case node.id of
    Just id ->
      selector == "#" ++ id
    Nothing ->
      False

matchesClass : String -> HtmlNode -> Bool
matchesClass selector node =
  case node.classes of
    Just classList ->
      List.member selector (List.map (\c -> "." ++ c) classList)
    Nothing ->
      False

takeNodes : List HtmlElement -> List HtmlNode
takeNodes =
  List.filterMap (
    \e ->
      case e of
        Node n ->
          Just n
        Text _ ->
          Nothing
  )

isSomething : Maybe a -> Bool
isSomething maybeSomething =
  case maybeSomething of
    Just _ ->
      True
    Nothing ->
      False

findFirstInList : (a -> Bool) -> List a -> Maybe a
findFirstInList matcher items =
  case List.head items of
    Just item ->
      if (matcher item) then
        Just item
      else
        findFirstInList matcher (List.drop 1 items)
    Nothing ->
      Nothing
