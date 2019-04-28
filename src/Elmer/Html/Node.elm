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
import Elmer.Value.Native as Native
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
  decodeHtml [] Nothing


decodeHtml : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Html msg -> HtmlNode msg
decodeHtml inheritedEvents tagger html =
  case Native.decode (htmlNode inheritedEvents tagger) html of
    Ok h ->
      h
    Err msg ->
      Debug.todo <| "Unable to parse html: " ++ (Json.errorToString msg)

{-|
-}
asElement : HtmlNode msg -> Maybe (HtmlElement msg)
asElement node =
  case node of
    Element el ->
      Just el
    Text _ ->
      Nothing


htmlNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
htmlNode inheritedEvents tagger =
  Json.field "$" Json.int
   |> Json.andThen (\ctor ->
        case ctor of
          0 ->
            textNode
          1 ->
            element inheritedEvents tagger
          2 ->
            element inheritedEvents tagger
          3 ->
            customElement inheritedEvents tagger
          4 ->
            taggedElement inheritedEvents tagger
          5 ->
            lazyElement inheritedEvents tagger
          unknownType ->
            Debug.todo <| "Unknown html type: " ++ (String.fromInt unknownType)
      )
    

textNode : Json.Decoder (HtmlNode msg)
textNode =
  Native.field "a"
    |> Json.map Text


taggedElement : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
taggedElement inheritedEvents maybePreviousTagger =
  Native.field "j"
    |> Json.andThen (\thisTagger ->
        let
          mappedTagger =
            maybePreviousTagger
              |> Maybe.map (\previous -> previous << thisTagger)
              |> Maybe.withDefault (Native.cast thisTagger)
        in
          Json.field "k" <|
            htmlNode inheritedEvents (Just mappedTagger)
      )


lazyElement : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
lazyElement inheritedEvents tagger =
  Json.field "m" Native.decoder
    |> Json.andThen (\lazyThunk ->
      lazyThunk ()
        |> Json.succeed << decodeHtml inheritedEvents tagger
    )  


element : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
element inheritedEvents tagger =
  Json.map Element <|
    Json.map7 HtmlElement
      tagDecoder
      propertiesDecoder
      attributesDecoder
      stylesDecoder
      (childrenDecoder inheritedEvents tagger)
      (Json.succeed inheritedEvents)
      (eventHandlers tagger)


customElement : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
customElement inheritedEvents tagger =
  Json.map Element <|
    Json.map7 HtmlElement
      (Json.succeed "div")
      propertiesDecoder
      attributesDecoder
      stylesDecoder
      (Json.succeed [ Text "<Custom Element -- Content not rendered>" ])
      (Json.succeed inheritedEvents)
      (eventHandlers tagger)


childrenDecoder : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (List (HtmlNode msg))
childrenDecoder inheritedEvents tagger =
  eventHandlers tagger
    |> Json.andThen (\events -> 
      childNodesDecoder (List.append inheritedEvents events) tagger
    )


childNodesDecoder : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (List (HtmlNode msg))
childNodesDecoder inheritedEvents tagger =
  Json.field "e" <| Json.list (
    Json.oneOf
    [ keyedNode inheritedEvents tagger
    , htmlNode inheritedEvents tagger 
    ]
  )


keyedNode : List (HtmlEventHandler msg) -> Maybe (Tagger subMsg msg) -> Json.Decoder (HtmlNode msg)
keyedNode inheritedEvents tagger =
  Json.map Tuple.second <| Value.tuple (Json.succeed ()) (htmlNode inheritedEvents tagger)


eventHandlers : Maybe (Tagger subMsg msg) -> Json.Decoder (List (HtmlEventHandler msg))
eventHandlers tagger =
  Json.oneOf
  [ Json.at [ "d", "a0" ]
      <| Json.map (List.map (mapTuple HtmlEventHandler))
      <| Json.keyValuePairs (eventValueDecoder tagger)
  , Json.succeed []
  ]


mapTuple : (a -> b -> c) -> (a, b) -> c
mapTuple mapper tuple =
  mapper (Tuple.first tuple) (Tuple.second tuple)


eventValueDecoder : Maybe (Tagger subMsg msg) -> Json.Decoder (Json.Decoder (HtmlEventValue msg))
eventValueDecoder tagger =
  case tagger of
    Just t ->
      decodeEventValue (\msg ->
        { message = t msg
        , stopPropagation = False
        , preventDefault = False
        }
      )
    Nothing ->
      Value.constructor
        |> Json.andThen (\ctor ->
            case ctor of
              "Custom" ->
                Native.field "a"
              "MayStopPropagation" ->
                decodeEventValue (\tup ->
                  { message = Tuple.first tup
                  , stopPropagation = Tuple.second tup
                  , preventDefault = False
                  }
                )
              "MayPreventDefault" ->
                decodeEventValue (\tup ->
                  { message = Tuple.first tup
                  , stopPropagation = False
                  , preventDefault = Tuple.second tup
                  }
                )
              "Normal" ->
                decodeEventValue (\msg ->
                  { message = msg
                  , stopPropagation = False
                  , preventDefault = False
                  }
                )
              unknown ->
                Json.fail <| "Unknown constructor for event: " ++ unknown
        )


decodeEventValue : (a -> HtmlEventValue msg) -> Json.Decoder (Json.Decoder (HtmlEventValue msg))
decodeEventValue mapper =
  Native.field "a"
    |> Json.map (Json.map mapper)


tagDecoder : Json.Decoder String
tagDecoder =
  Json.field "c" Json.string


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
  Json.oneOf
  [ Json.at [ "d", "a3" ] <| Json.dict Json.string
  , Json.succeed Dict.empty
  ]


stylesDecoder : Json.Decoder (Dict String String)
stylesDecoder =
  Json.oneOf
  [ Json.at [ "d", "a1" ] <| Json.dict Json.string
  , Json.succeed Dict.empty
  ]
