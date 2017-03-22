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
  facts element
    |> Dict.toList
    |> List.filterMap (\(key, fact) ->
      case fact of
        StringValue value ->
          Just (key, value)
        BoolValue value ->
          Just (key, Internal.boolToString value)
        DictValue _ ->
          Nothing
      )
    |> Dict.fromList

styles : HtmlElement msg -> Maybe (Dict String String)
styles element =
  case Dict.get "STYLE" <| facts element of
    Just (DictValue styles) ->
      Just styles
    Just _ ->
      Nothing
    Nothing ->
      Nothing


attributes : HtmlElement msg -> Dict String String
attributes element =
  Result.withDefault Dict.empty <|
      Json.decodeString (Json.field "ATTR" (Json.dict Json.string)) element.facts

attribute : String -> HtmlElement msg -> Maybe String
attribute name element =
  attributes element
    |> Dict.get name

factsDecoder : Json.Decoder HtmlFact
factsDecoder =
  Json.oneOf
    [ Json.map StringValue Json.string
    , Json.map BoolValue Json.bool
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
    BoolValue value ->
      boolFactToString (key, value)
    DictValue value ->
      Dict.toList value
        |> List.map stringFactToString
        |> String.join ", "

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
