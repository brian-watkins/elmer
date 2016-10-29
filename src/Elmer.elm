module Elmer exposing
  ( componentState
  , find
  , findNode
  , expectNode
  , expectNodeExists
  , id
  , classList
  , customAttributesDict
  , map
  , HtmlElement(..)
  , HtmlNode
  , HtmlEvent
  , HtmlComponentState
  , ComponentStateResult(..)
  )

import Html exposing (Html)
import Native.Helpers
import String
import Dict exposing (Dict)
import Json.Decode as Json exposing ((:=))
import Regex exposing (Regex)
import Expect
import Maybe.Extra as MaybeEx

type HtmlElement =
  Node HtmlNode |
  Text String

type alias HtmlNode =
  { tag: String
  , facts: String
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
find selector =
  map (updateTargetNode selector)

updateTargetNode : String -> HtmlComponentState model msg -> ComponentStateResult model msg
updateTargetNode selector componentState =
  case findNode (componentState.view componentState.model) selector of
    Just node ->
      CurrentState { componentState | targetNode = Just node }
    Nothing ->
      UpstreamFailure ("No html node found with selector: " ++ selector)

findNode : Html msg -> String -> Maybe HtmlNode
findNode html selector =
  Native.Helpers.asHtmlNode html `Maybe.andThen` (findWithinNode selector)

findWithinNode : String -> HtmlNode -> Maybe HtmlNode
findWithinNode selector root =
  if matchesNode selector root then
    Just root
  else
    List.head <|
      List.filterMap (findWithinNode selector) (takeNodes root.children)

matchesNode : String -> HtmlNode -> Bool
matchesNode selector node =
  case String.uncons selector of
    Just (selectorType, name) ->
      case selectorType of
        '#' ->
          matchesId name node
        '.' ->
          matchesClass name node
        _ ->
          matchesTagSelector (tagSelector selector) node
    Nothing ->
      False

matchesTagSelector : TagSelector -> HtmlNode -> Bool
matchesTagSelector tagSelector node =
  case tagSelector.tag of
    Just tagName ->
      if matchesTag tagName node then
        Maybe.withDefault True <|
          matchAttributeSelector tagSelector node
      else
        False
    Nothing ->
      Maybe.withDefault False <|
        matchAttributeSelector tagSelector node

matchAttributeSelector : TagSelector -> HtmlNode -> Maybe Bool
matchAttributeSelector tagSelector node =
  Maybe.map
    (\attrName -> matchesAttribute attrName tagSelector.attributeValue node)
    tagSelector.attributeName

type alias TagSelector =
  { tag: Maybe String
  , attributeName: Maybe String
  , attributeValue: Maybe String
  }

emptyTagSelector : TagSelector
emptyTagSelector =
  { tag = Nothing
  , attributeName = Nothing
  , attributeValue = Nothing
  }

tagSelector : String -> TagSelector
tagSelector selector =
  let
    matchMaybe = List.head <|
      Regex.find (Regex.AtMost 1)
        (Regex.regex "^([\\w-]*)(?:\\[([\\w-]+)(?:='([\\w-]+)')?\\])?")
        selector
  in
    case matchMaybe of
      Just match ->
        { tag = submatch 0 match
        , attributeName = submatch 1 match
        , attributeValue = submatch 2 match
        }
      Nothing ->
        emptyTagSelector

submatch : Int -> Regex.Match -> Maybe String
submatch index match =
  notEmpty << MaybeEx.join << List.head << List.drop index <| match.submatches

notEmpty : Maybe String -> Maybe String
notEmpty maybeEmpty =
  maybeEmpty `Maybe.andThen` (\s -> if String.isEmpty s then Nothing else Just s)

matchesId : String -> HtmlNode -> Bool
matchesId selector node =
  Maybe.withDefault False (Maybe.map ((==) selector) (id node))

matchesClass : String -> HtmlNode -> Bool
matchesClass selector node =
  List.member selector (classList node)

matchesTag : String -> HtmlNode -> Bool
matchesTag selector node =
  node.tag == selector

matchesAttribute : String -> Maybe String -> HtmlNode -> Bool
matchesAttribute attributeName maybeAttributeValue node =
  let
    attributesDict = customAttributesDict node
  in
    Maybe.withDefault (Dict.member attributeName attributesDict) <|
      Maybe.map
        ((==) (Maybe.withDefault "" (Dict.get attributeName attributesDict)))
        maybeAttributeValue

id : HtmlNode -> Maybe String
id node =
  Result.toMaybe (Json.decodeString ("id" := Json.string) node.facts)

classList : HtmlNode -> List String
classList node =
  case Json.decodeString ("className" := Json.string) node.facts of
    Ok classes ->
      String.split " " classes
    Err _ ->
      []

customAttributesDict : HtmlNode -> Dict String String
customAttributesDict node =
  Result.withDefault Dict.empty <|
    Json.decodeString ("ATTR" := (Json.dict Json.string)) node.facts

takeNodes : List HtmlElement -> List HtmlNode
takeNodes =
  List.filterMap (
    \e ->
      case e of
        Node n ->
          Just n
        _ ->
          Nothing
  )
