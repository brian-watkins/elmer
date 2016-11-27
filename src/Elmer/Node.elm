module Elmer.Node exposing
  ( toString
  )

import Elmer exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)

toString : HtmlNode msg -> String
toString node =
  (printElement "" (Node node))

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
