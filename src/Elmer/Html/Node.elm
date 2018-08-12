module Elmer.Html.Node exposing
  ( from
  , asElement
  )

{-| Node (exposed for testing)

-- @docs from, asElement

-}


import Html exposing (Html)
import Elmer.Html.Types exposing (..)
import Elmer.Value as Value
import Json.Decode as Json
import Dict exposing (Dict)


type alias Tagger a b =
  (a -> b)

type NodeType
  = Normal
  | Keyed

{-|
-}
from : Html msg -> HtmlNode msg
from =
  fromHtml [] Nothing

{-|
-}
asElement : HtmlNode msg -> Maybe (HtmlElement msg)
asElement node =
  case node of
    Element element ->
      Just element
    Text _ ->
      Nothing


-- REVISIT: What about 'custom' nodes (I think that's what case 3 is ...). No tests for this as of now I think
fromHtml : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromHtml inheritedEvents tagger html =
  -- let
    -- d = Elm.Kernel.Value.print "html" html
  -- in
  case Value.field "$" html of
    0 ->
      fromText html
    1 ->
      fromNode Normal inheritedEvents tagger html
    2 ->
      fromNode Keyed inheritedEvents tagger html
    4 ->
      fromTagger inheritedEvents tagger html
    5 -> 
      ()
        |> Value.field "m" html
        |> fromHtml inheritedEvents tagger
    unknownType ->
      Debug.todo <| "Unknown html type: " ++ (String.fromInt unknownType)


fromText : Html msg -> HtmlNode msg
fromText html =
  Value.field "a" html
    |> Text


fromTagger : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromTagger inheritedEvents maybePreviousTagger html =
  let
    thisTagger = Value.field "j" html
    fullTagger =
      maybePreviousTagger
        |> Maybe.map (\previous -> previous << thisTagger)
        |> Maybe.withDefault thisTagger
  in
    Value.field "k" html
      |> fromHtml inheritedEvents (Just fullTagger)


fromNode : NodeType -> List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromNode nodeType inheritedEvents tagger node =
  let
    nodeEvents = eventHandlers tagger node
    eventsToInherit =
      List.append inheritedEvents nodeEvents
  in
    Element
      { tag = Value.field "c" node
      , properties = decodeDict propertiesDecoder node
      , attributes = decodeDict attributesDecoder node
      , styles = decodeDict stylesDecoder node
      , children = 
          case nodeType of
            Normal ->
              handleChildNodes identity eventsToInherit tagger node
            Keyed ->
              handleChildNodes Tuple.second eventsToInherit tagger node
      , inheritedEventHandlers = inheritedEvents
      , eventHandlers = nodeEvents
      }


handleChildNodes : (a -> Html msg) -> List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> List (HtmlNode msg)
handleChildNodes mapper inheritedEvents tagger node =
  Value.decode childrenDecoder node
    |> Result.withDefault []
    |> List.map mapper
    |> List.map (fromHtml inheritedEvents tagger)


childrenDecoder : Json.Decoder (List Json.Value)
childrenDecoder =
  Json.field "e" <| Json.list Value.decoder


childDecoder : Json.Decoder Json.Value
childDecoder =
  Json.oneOf
    [ Json.field "_1" Json.value
    , Json.value
    ]

eventHandlers : Maybe (Tagger subMsg msg) -> Html msg -> List (HtmlEventHandler msg)
eventHandlers tagger html =
  Value.decode eventDecoder html
    |> Result.withDefault Dict.empty
    |> Dict.toList
    |> List.map (toEventHandler tagger)


eventDecoder : Json.Decoder (Dict String Json.Value)
eventDecoder =
  Json.dict Value.decoder
    |> Json.at [ "d", "a0" ]


toEventHandler : Maybe (Tagger subMsg msg) -> (String, Json.Value) -> HtmlEventHandler msg
toEventHandler tagger (name, event) =
  { eventType = name
  , decoder =
      case tagger of
        Just t ->
          Json.map (\m -> { message = t m, stopPropagation = False, preventDefault = False }) <|
                Value.field "a" event
        Nothing ->
          case Value.constructor event of
            "Custom" ->
              Value.field "a" event
            "MayStopPropagation" ->
              Json.map (\t -> { message = Tuple.first t, stopPropagation = Tuple.second t, preventDefault = False }) <|
                Value.field "a" event
            "MayPreventDefault" ->
              Json.map (\t -> { message = Tuple.first t, stopPropagation = False, preventDefault = Tuple.second t }) <|
                Value.field "a" event
            "Normal" ->
              Json.map (\m -> { message = m, stopPropagation = False, preventDefault = False }) <|
                Value.field "a" event
            constructor ->
              Json.fail <| "Unknown constructor for event: " ++ constructor
  }


decodeDict : Json.Decoder (Dict String a) -> Html msg -> Dict String a
decodeDict decoder node =
  Value.decode decoder node
    |> Result.withDefault Dict.empty


propertiesDecoder : Json.Decoder (Dict String HtmlFact)
propertiesDecoder =
  Json.oneOf
    [ Json.map (\v -> Just (StringValue v)) Json.string
    , Json.map (\v -> Just (BoolValue v)) Json.bool
    , Json.succeed Nothing
    ]
      |> Json.dict
      |> Json.map (\d ->
        Dict.foldl (\key value result -> 
            case value of
              Just val ->
                Dict.insert key val result
              Nothing ->
                result
          ) Dict.empty d
      )
      |> Json.field "d"


-- REVISIT -- Could attributes have boolean value or number values? Do we care?
attributesDecoder : Json.Decoder (Dict String String)
attributesDecoder =
    Json.dict Json.string
      |> Json.at [ "d", "a3" ]


stylesDecoder : Json.Decoder (Dict String String)
stylesDecoder =
    Json.dict Json.string
      |> Json.at [ "d", "a1" ]
