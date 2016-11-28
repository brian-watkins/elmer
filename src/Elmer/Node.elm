module Elmer.Node exposing
  ( findNode
  , toString
  , classList
  , id
  , property
  )

import Elmer.Types exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)
import Regex exposing (Regex)
import Maybe.Extra as MaybeEx
import Html exposing (Html)


toString : HtmlNode msg -> String
toString node =
  (printElement "" (Node node))

findNode : Html msg -> String -> Maybe (HtmlNode msg)
findNode html selector =
    Native.Helpers.asHtmlNode html |> Maybe.andThen (findWithinNode selector)

id : HtmlNode msg -> Maybe String
id =
  property "id"

classList : HtmlNode msg -> List String
classList node =
    case property "className" node of
        Just classes ->
            String.split " " classes

        Nothing ->
            []

property : String -> HtmlNode msg -> Maybe String
property name node =
  Json.decodeString (Json.field name Json.string) node.facts
    |> Result.toMaybe


findWithinNode : String -> HtmlNode msg -> Maybe (HtmlNode msg)
findWithinNode selector root =
    if matchesNode selector root then
        Just root
    else
        List.head <|
            List.filterMap (findWithinNode selector) (takeNodes root.children)


matchesNode : String -> HtmlNode msg -> Bool
matchesNode selector node =
    case String.uncons selector of
        Just ( selectorType, name ) ->
            case selectorType of
                '#' ->
                    matchesId name node

                '.' ->
                    matchesClass name node

                _ ->
                    matchesTagSelector (tagSelector selector) node

        Nothing ->
            False


matchesTagSelector : TagSelector -> HtmlNode msg -> Bool
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


matchAttributeSelector : TagSelector -> HtmlNode msg -> Maybe Bool
matchAttributeSelector tagSelector node =
    Maybe.map
        (\attrName -> matchesAttribute attrName tagSelector.attributeValue node)
        tagSelector.attributeName


type alias TagSelector =
    { tag : Maybe String
    , attributeName : Maybe String
    , attributeValue : Maybe String
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
        matchMaybe =
            List.head <|
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
    maybeEmpty
        |> Maybe.andThen
            (\s ->
                if String.isEmpty s then
                    Nothing
                else
                    Just s
            )


matchesId : String -> HtmlNode msg -> Bool
matchesId selector node =
    Maybe.withDefault False (Maybe.map ((==) selector) (id node))


matchesClass : String -> HtmlNode msg -> Bool
matchesClass selector node =
    List.member selector (classList node)


matchesTag : String -> HtmlNode msg -> Bool
matchesTag selector node =
    node.tag == selector


matchesAttribute : String -> Maybe String -> HtmlNode msg -> Bool
matchesAttribute attributeName maybeAttributeValue node =
    let
        attributesDict =
            customAttributesDict node
    in
        Maybe.withDefault (Dict.member attributeName attributesDict) <|
            Maybe.map
                ((==) (Maybe.withDefault "" (Dict.get attributeName attributesDict)))
                maybeAttributeValue




customAttributesDict : HtmlNode msg -> Dict String String
customAttributesDict node =
    Result.withDefault Dict.empty <|
        Json.decodeString (Json.field "ATTR" (Json.dict Json.string)) node.facts


takeNodes : List (HtmlElement msg) -> List (HtmlNode msg)
takeNodes =
    List.filterMap
        (\e ->
            case e of
                Node n ->
                    Just n

                _ ->
                    Nothing
        )


printElement : String -> HtmlElement msg -> String
printElement indentation element =
  case element of
    Node node ->
      let
        childIndentation = indentation ++ "  "
        facts = printFacts node
        events = printEvents node
        children = List.map (printElement childIndentation) node.children
          |> String.join "\n"
      in
        indentation ++ "- " ++ node.tag ++ " " ++ facts ++ " " ++ events ++ "\n"
          ++ children
    Text text ->
      indentation ++ "- " ++ text

printEvents : HtmlNode msg -> String
printEvents node =
  if List.isEmpty node.events then
    ""
  else
    let
      eventString = List.map .eventType node.events
        |> String.join ", "
    in
      "[ " ++ eventString ++ " ]"

printFacts : HtmlNode msg -> String
printFacts node =
  let
    facts = Json.decodeString (Json.dict factsDecoder) node.facts
      |> Result.withDefault Dict.empty
      |> Dict.toList
  in
    if List.isEmpty facts then
      ""
    else
      let
        factString = List.map factToString facts
          |> String.join ", "
      in
        "{ " ++ factString ++ " }"

type HtmlFact
  = StringValue String
  | DictValue (Dict String String)

factsDecoder : Json.Decoder HtmlFact
factsDecoder =
  Json.oneOf
    [ Json.map StringValue Json.string
    , Json.map DictValue (Json.dict Json.string)
    ]

factToString : (String, HtmlFact) -> String
factToString (key, fact) =
  case fact of
    StringValue value ->
      stringFactToString (key, value)
    DictValue value ->
      Dict.toList value
        |> List.map stringFactToString
        |> String.join ", "

stringFactToString : (String, String) -> String
stringFactToString (key, value) =
  key ++ " = '" ++ value ++ "'"
