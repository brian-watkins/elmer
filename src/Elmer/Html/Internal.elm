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
  element.facts
    |> Dict.toList
    |> List.filterMap (\(key, fact) ->
      case fact of
        StringValue value ->
          Just (key, value)
        BoolValue value ->
          Just (key, Internal.boolToString value)
        DictValue _ ->
          Nothing
        Ignored ->
          Nothing
      )
    |> Dict.fromList


styles : HtmlElement msg -> Maybe (Dict String String)
styles element =
  case Dict.get "STYLE" element.facts of
    Just (DictValue styles) ->
      Just styles
    Just _ ->
      Nothing
    Nothing ->
      Nothing


attributes : HtmlElement msg -> Dict String String
attributes element =
  case Dict.get "ATTR" element.facts of
    Just (DictValue attrs) ->
      attrs
    _ ->
      Dict.empty


attribute : String -> HtmlElement msg -> Maybe String
attribute name element =
  attributes element
    |> Dict.get name


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
  if List.isEmpty node.eventHandlers then
    ""
  else
    let
      eventString = List.map .eventType node.eventHandlers
        |> String.join ", "
    in
      "[ " ++ eventString ++ " ]"


printFacts : HtmlElement msg -> String
printFacts node =
  let
    factsList =
      node.facts
        |> Dict.toList
  in
    if List.isEmpty factsList then
      ""
    else
      let
        factString =
          List.filterMap factToString factsList
            |> String.join ", "
      in
        if String.isEmpty factString then
          ""
        else
          "{ " ++ factString ++ " }"


factToString : (String, HtmlFact) -> Maybe String
factToString (key, fact) =
  case fact of
    StringValue value ->
      Just <| stringFactToString (key, value)
    BoolValue value ->
      Just <| boolFactToString (key, value)
    DictValue value ->
      Dict.toList value
        |> List.map stringFactToString
        |> String.join ", "
        |> Just
    Ignored ->
      Nothing


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
