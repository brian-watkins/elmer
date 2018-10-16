module Elmer.Html.Element.Printer exposing
  ( print
  )

{-| Exposed for testing

@docs print

-}

import Elmer.Internal as Internal
import Elmer.Html.Types exposing (..)
import Dict


{-|
-}
print : HtmlElement msg -> String
print node =
  (printElement "" (Element node))


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
printEvents element =
  if List.isEmpty element.eventHandlers then
    ""
  else
    let
      eventString = List.map .eventType element.eventHandlers
        |> String.join ", "
    in
      "[ " ++ eventString ++ " ]"


printFacts : HtmlElement msg -> String
printFacts element =
  let
    factsList =
      attributesToStrings element ++
      propertiesToStrings element ++
      stylesToString element
  in
    if List.isEmpty factsList then
      ""
    else
      let
        factString = String.join ", " factsList
      in
        if String.isEmpty factString then
          ""
        else
          "{ " ++ factString ++ " }"


propertiesToStrings : HtmlElement msg -> List String
propertiesToStrings element =
  element.properties
    |> Dict.toList
    |> List.map factToString


factToString : (String, HtmlFact) -> String
factToString (key, fact) =
  case fact of
    StringValue value ->
      stringFactToString (key, value)
    BoolValue value ->
      boolFactToString (key, value)


attributesToStrings : HtmlElement msg -> List String
attributesToStrings element =
  element.attributes
    |> Dict.toList
    |> List.map stringFactToString
    

stringFactToString : (String, String) -> String
stringFactToString (key, value) =
  key ++ " = '" ++ value ++ "'"


boolFactToString : (String, Bool) -> String
boolFactToString (key, value) =
  key ++ " = " ++ Internal.boolToString value


stylesToString : HtmlElement msg -> List String
stylesToString element =
  let
    styleString = 
      element.styles
        |> Dict.toList
        |> List.map styleFactToString
        |> String.join "; "
  in
    if String.isEmpty styleString then
      []
    else
      [ stringFactToString ("style", styleString) ]
  

styleFactToString : (String, String) -> String
styleFactToString (key, value) =
  key ++ ": " ++ value