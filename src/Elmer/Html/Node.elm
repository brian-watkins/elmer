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


fromHtml : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromHtml inheritedEvents tagger html =
  case Value.field "$" html of
    0 ->
      fromText html
    1 ->
      fromNode inheritedEvents tagger html
    2 ->
      fromKeyedNode inheritedEvents tagger html
    3 ->
      fromCustomNode inheritedEvents tagger html
    4 ->
      fromTaggedNode inheritedEvents tagger html
    5 -> 
      fromLazyNode inheritedEvents tagger html
    unknownType ->
      Debug.todo <| "Unknown html type: " ++ (String.fromInt unknownType)


fromText : Html msg -> HtmlNode msg
fromText html =
  Value.field "a" html
    |> Text


fromTaggedNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromTaggedNode inheritedEvents maybePreviousTagger html =
  let
    thisTagger = Value.field "j" html
    fullTagger =
      maybePreviousTagger
        |> Maybe.map (\previous -> previous << thisTagger)
        |> Maybe.withDefault thisTagger
  in
    Value.field "k" html
      |> fromHtml inheritedEvents (Just fullTagger)


fromLazyNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromLazyNode inheritedEvents tagger html =
  ()
    |> Value.field "m" html
    |> fromHtml inheritedEvents tagger


fromNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromNode =
  fromNodeWithChildren identity


fromKeyedNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromKeyedNode =
  fromNodeWithChildren Tuple.second
  

fromNodeWithChildren : (a -> Html msg) -> List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromNodeWithChildren childMapper inheritedEvents tagger node =
  basicElement (Value.field "c" node) inheritedEvents tagger node
    |> withChildren (
        handleChildNodes 
          childMapper
          ( List.append inheritedEvents <| eventHandlers tagger node )
          tagger
          node
    )
    |> Element


fromCustomNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
fromCustomNode inheritedEvents tagger html =
  basicElement "div" inheritedEvents tagger html
    |> withChildren [ Text "<Custom Element -- Content not rendered>" ]
    |> Element


basicElement : String -> List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlElement msg
basicElement tag inheritedEvents tagger html =
  { tag = tag
  , properties = decodeDict propertiesDecoder html
  , attributes = decodeDict attributesDecoder html
  , styles = decodeDict stylesDecoder html
  , children = []
  , inheritedEventHandlers = inheritedEvents
  , eventHandlers = eventHandlers tagger html
  }


withChildren : List (HtmlNode msg) -> HtmlElement msg -> HtmlElement msg
withChildren children element =
  { element | children = children }


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
