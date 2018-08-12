module Elmer.Html.Internal exposing
  ( toString
  , elementId
  , classList
  , attributes
  , attribute
  , properties
  , property
  , styles
  , hasProperty
  , isCheckbox
  , isSubmitButton
  , isSubmitInput
  )

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Elmer.Internal as Internal
import Dict exposing (Dict)


toString : HtmlElement msg -> String
toString node =
  (printElement "" (Element node))


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
property name element =
  properties element
    |> Dict.get name


hasProperty : (String, String) -> HtmlElement msg -> Bool
hasProperty (key, value) element =
  property key element
    |> Maybe.withDefault ""
    |> (==) value


properties : HtmlElement msg -> Dict String String
properties element =
  element.properties
    |> Dict.toList
    |> List.filterMap (\(key, fact) ->
      case fact of
        StringValue value ->
          Just (key, value)
        BoolValue value ->
          Just (key, Internal.boolToString value)
      )
    |> Dict.fromList


styles : HtmlElement msg -> Dict String String
styles element =
  element.styles


attributes : HtmlElement msg -> Dict String String
attributes element =
  element.attributes


attribute : String -> HtmlElement msg -> Maybe String
attribute name element =
  attributes element
    |> Dict.get name


-- REVISIT: What about styles? We don't seem to print out the styles of an element? Or don't have a test for that?
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
      propertiesToStrings element
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


isCheckbox : HtmlElement msg -> Bool
isCheckbox element =
  element.tag == "input" &&
    ( property "type" element |> Maybe.withDefault "" ) == "checkbox"


isSubmitInput : HtmlElement msg -> Bool
isSubmitInput element =
  element.tag == "input" &&
    hasProperty ("type", "submit") element


isSubmitButton : HtmlElement msg -> Bool
isSubmitButton element =
  element.tag == "button" &&
    ( hasProperty ("type", "") element ||
      hasProperty ("type", "submit") element
    )
