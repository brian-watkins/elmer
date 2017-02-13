module Elmer.Html.Element exposing
  ( id
  , classList
  , property
  , properties
  , attributes
  , toString
  )

{-| Functions for working directly with HtmlElements.

# Element Characteristics
@docs id, classList, property, properties, attributes

# Debugging
@docs toString

-}

import Elmer.Html.Types exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)
import Html exposing (Html)

{-| Represent an `HtmlElement` as a String.
-}
toString : HtmlElement msg -> String
toString node =
  (printElement "" (Element node))

{-| Get the value of the element's `id` attribute, if it is defined.
-}
id : HtmlElement msg -> Maybe String
id =
  property "id"

{-| Get a list of classes applied to this element.
-}
classList : HtmlElement msg -> List String
classList node =
    case property "className" node of
        Just classes ->
            String.split " " classes

        Nothing ->
            []

{-| Get the value of a particular property belonging to this
element, if that property is defined.
-}
property : String -> HtmlElement msg -> Maybe String
property name node =
  Json.decodeString (Json.field name Json.string) node.facts
    |> Result.toMaybe

{-| Get this element's properties as a `Dict`.

On the difference between attributes and properties,
see [this](https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md).
-}
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

{-| Get this element's attributes as a `Dict`. If you define a custom attribute
for an Html element, you can find it with this function.

    componentState
      |> expectElement (\element ->
        attributes element
          |> Dict.get "data-attribute"
          |> Expect.notEqual Nothing
      )
-}
attributes : HtmlElement msg -> Dict String String
attributes node =
    Result.withDefault Dict.empty <|
        Json.decodeString (Json.field "ATTR" (Json.dict Json.string)) node.facts



facts : HtmlElement msg -> Dict String HtmlFact
facts node =
  Json.decodeString (Json.dict factsDecoder) node.facts
    |> Result.withDefault Dict.empty

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
