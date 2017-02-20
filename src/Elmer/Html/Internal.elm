module Elmer.Html.Internal exposing
  ( toString
  , elementId
  , classList
  , attributes
  , properties
  , property
  )

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Dict exposing (Dict)

toString : HtmlElement msg -> String
toString node =
  (printElement "" (Element node))

facts : HtmlElement msg -> Dict String HtmlFact
facts node =
  Json.decodeString (Json.dict factsDecoder) node.facts
    |> Result.withDefault Dict.empty

elementId : HtmlElement msg -> Maybe String
elementId =
  property "id"

classList : HtmlElement msg -> List String
classList node =
    case property "className" node of
        Just classes ->
            String.split " " classes

        Nothing ->
            []

property : String -> HtmlElement msg -> Maybe String
property name node =
  Json.decodeString (Json.field name Json.string) node.facts
    |> Result.toMaybe

properties : HtmlElement msg -> Dict String String
properties node =
  facts node
    |> Dict.toList
    |> List.filterMap (\(key, fact) ->
      case fact of
        StringValue value ->
          Just (key, value)
        DictValue _ ->
          Nothing
      )
    |> Dict.fromList

attributes : HtmlElement msg -> Dict String String
attributes node =
    Result.withDefault Dict.empty <|
        Json.decodeString (Json.field "ATTR" (Json.dict Json.string)) node.facts



factsDecoder : Json.Decoder HtmlFact
factsDecoder =
  Json.oneOf
    [ Json.map StringValue Json.string
    , Json.map DictValue (Json.dict Json.string)
    ]


printElement : String -> HtmlNode msg -> String
printElement indentation element =
  case element of
    Element node ->
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

printEvents : HtmlElement msg -> String
printEvents node =
  if List.isEmpty node.events then
    ""
  else
    let
      eventString = List.map .eventType node.events
        |> String.join ", "
    in
      "[ " ++ eventString ++ " ]"


printFacts : HtmlElement msg -> String
printFacts node =
  let
    factsList = facts node
      |> Dict.toList
  in
    if List.isEmpty factsList then
      ""
    else
      let
        factString = List.map factToString factsList
          |> String.join ", "
      in
        "{ " ++ factString ++ " }"


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
