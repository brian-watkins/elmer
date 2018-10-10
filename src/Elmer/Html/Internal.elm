module Elmer.Html.Internal exposing
  ( tag
  , elementId
  , classList
  , allAttrs
  , attributes
  , attribute
  , properties
  , property
  , styles
  , hasProperty
  , isCheckbox
  , isSubmitButton
  , isSubmitInput
  , texts
  , childElements
  )

import Json.Decode as Json
import Elmer.Html.Types exposing (..)
import Elmer.Internal as Internal
import Dict exposing (Dict)


tag : HtmlElement msg -> String
tag element =
  element.tag


texts : HtmlElement msg -> List String
texts element =
  List.filterMap (\el ->
    case el of
      Element _ ->
        Nothing
      Text text ->
        Just text
  ) element.children


childElements : HtmlElement msg -> List (HtmlElement msg)
childElements element =
  List.filterMap (\el ->
    case el of
      Element child ->
        Just child
      Text _ ->
        Nothing
  ) element.children


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


allAttrs : HtmlElement msg -> Dict String String
allAttrs element =
  Dict.union (attributes element) (properties element)


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
