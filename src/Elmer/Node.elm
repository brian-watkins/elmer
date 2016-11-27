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
      in
        indentation ++ "- " ++ node.tag ++ " " ++ (printFacts node) ++ " " ++ (printEvents node) ++ "\n"
          ++ (String.join ("\n") (List.map (printElement childIndentation) node.children))
    Text text ->
      indentation ++ "- " ++ text

printEvents : HtmlNode msg -> String
printEvents node =
  if List.isEmpty node.events then
    ""
  else
    "[ " ++ (String.join ", " (List.map .eventType node.events)) ++ " ]"

printFacts : HtmlNode msg -> String
printFacts node =
  let
    facts = Result.withDefault Dict.empty (Json.decodeString (Json.dict Json.string) node.facts)
  in
    if Dict.isEmpty facts then
      ""
    else
      "{ " ++ (String.join ", " (List.map (\(k, v) -> k ++ " = '" ++ v ++ "'") (Dict.toList facts))) ++ " }"
