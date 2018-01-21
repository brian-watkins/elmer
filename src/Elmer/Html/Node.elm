module Elmer.Html.Node exposing
  ( from
  , asElement
  )

import Html exposing (Html)
import Elmer.Html.Types exposing (..)
import Elmer.Value as Value
import Json.Decode as Json
import Dict exposing (Dict)


type alias Tagger a b =
  (a -> b)


from : Html msg -> HtmlNode msg
from =
  fromHtml [] Nothing


asElement : HtmlNode msg -> Maybe (HtmlElement msg)
asElement node =
  case node of
    Element element ->
      Just element
    Text _ ->
      Nothing


fromHtml : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromHtml inheritedEvents tagger html =
  case Value.field "type" html of
    "text" ->
      fromText html
    "node" ->
      fromNode inheritedEvents tagger html
    "tagger" ->
      fromTagger inheritedEvents html
    "thunk" ->
      ()
        |> Value.field "thunk" html
        |> fromHtml inheritedEvents tagger
    unknownType ->
      Debug.crash <| "Unknown html type: " ++ unknownType


fromText : Html msg -> HtmlNode msg
fromText html =
  Value.field "text" html
    |> Text


fromTagger : List (HtmlEventHandler msg) -> Html msg -> HtmlNode msg
fromTagger inheritedEvents html =
  Value.field "node" html
    |> fromHtml inheritedEvents (Just <| Value.field "tagger" html)


fromNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromNode inheritedEvents tagger node =
  let
    nodeEvents = eventHandlers tagger node
    eventsToInherit =
      List.append inheritedEvents nodeEvents
  in
    Element
      { tag = Value.field "tag" node
      , facts = facts node
      , children = handleChildNodes eventsToInherit tagger node
      , inheritedEventHandlers = inheritedEvents
      , eventHandlers = nodeEvents
      }


handleChildNodes : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> List (HtmlNode msg)
handleChildNodes inheritedEvents tagger node =
  Value.decode childrenDecoder node
    |> Result.withDefault []
    |> List.map (fromHtml inheritedEvents tagger)


childrenDecoder : Json.Decoder (List Json.Value)
childrenDecoder =
  Json.field "children" <| Json.list Json.value


eventHandlers : Maybe (Tagger subMsg msg) -> Html msg -> List (HtmlEventHandler msg)
eventHandlers tagger html =
  Value.decode eventDecoder html
    |> Result.withDefault Dict.empty
    |> Dict.toList
    |> List.map (toEventHandler tagger)


eventDecoder : Json.Decoder (Dict String Json.Value)
eventDecoder =
  Json.dict Json.value
    |> Json.at [ "facts", "EVENT" ]


toEventHandler : Maybe (Tagger subMsg msg) -> (String, Json.Value) -> HtmlEventHandler msg
toEventHandler tagger (name, event) =
  { eventType = name
  , options = Value.field "options" event
  , decoder =
      case tagger of
        Just t ->
          Value.field "decoder" event
            |> Json.map t
        Nothing ->
          Value.field "decoder" event
  }


facts : Html msg -> Dict String HtmlFact
facts node =
  Value.decode factsDecoder node
    |> Result.withDefault Dict.empty


factsDecoder : Json.Decoder (Dict String HtmlFact)
factsDecoder =
  Json.oneOf
    [ Json.map StringValue Json.string
    , Json.map BoolValue Json.bool
    , Json.map DictValue (Json.dict Json.string)
    , Json.succeed <| Ignored
    ]
  |> Json.dict
  |> Json.field "facts"
