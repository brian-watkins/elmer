module Elmer.TestApps.MouseTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events exposing
  ( onClick
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  )
import Json.Decode as Json

type alias Model =
  { clicks : Int
  , doubleClicks : Int
  , mouseDowns : Int
  , mouseUps : Int
  , mouseEnters : Int
  , mouseLeaves : Int
  , mouseOvers : Int
  , mouseOuts : Int
  , position : Maybe MousePosition
  }

type Msg
  = DoClick
  | DoDoubleClick
  | DoMouseDown
  | DoMouseUp
  | DoMouseEnter
  | DoMouseLeave
  | DoMouseOver
  | DoMouseOut
  | RecordPosition MousePosition

type alias MousePosition =
  { x: Int
  , y: Int
  }

defaultModel : Model
defaultModel =
  { clicks = 0
  , doubleClicks = 0
  , mouseDowns = 0
  , mouseUps = 0
  , mouseEnters = 0
  , mouseLeaves = 0
  , mouseOvers = 0
  , mouseOuts = 0
  , position = Nothing
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root", Attr.class "no-events"]
    [ Html.div
      [ Attr.class "button"
      , onClick DoClick
      , onDoubleClick DoDoubleClick
      , onMouseDown DoMouseDown
      , onMouseUp DoMouseUp
      , onMouseOver DoMouseOver
      , onMouseOut DoMouseOut
      ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((String.fromInt model.clicks) ++ " clicks!") ]
    ]

viewForPosition : Model -> Html Msg
viewForPosition model =
  Html.div
    [ Attr.id "root"
    ]
    [ Html.div
      [ Attr.class "button"
      , onMouseEvent "click" RecordPosition
      , onMouseEvent "mousedown" RecordPosition
      , onMouseEvent "mouseup" RecordPosition
      , onMouseEvent "mouseover" RecordPosition
      , onMouseEvent "mouseout" RecordPosition
      ]
      [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((String.fromInt model.clicks) ++ " clicks!") ]
    , Html.div
      [ Attr.id "enter-leave-element"
      , onMouseEvent "mouseenter" RecordPosition
      , onMouseEvent "mouseleave" RecordPosition
      ]
      [ Html.div [ Attr.id "child-element" ] [ Html.text "Mouse over me please!" ] ]
    ]

viewForMouseEnterLeave : Model -> Html Msg
viewForMouseEnterLeave model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.class "no-events" ] [ Html.text "no events" ]
    , Html.div [ Attr.id "event-parent", onMouseEnter DoMouseEnter, onMouseLeave DoMouseLeave ]
      [ Html.ul []
        [ Html.li
          [ Attr.attribute "data-option" "1"
          , onMouseEnter DoMouseEnter
          , onMouseLeave DoMouseLeave
          ] [ Html.text "Option 1" ]
        , Html.li [ Attr.attribute "data-option" "2" ] [ Html.text "Option 2" ]
        , Html.li [ Attr.attribute "data-option" "3" ] [ Html.text "Option 3" ]
        ]
      ]
    ]

onMouseEvent : String -> (MousePosition -> Msg) -> Html.Attribute Msg
onMouseEvent eventType tagger =
  Events.on eventType <| Json.map tagger mousePositionDecoder

mousePositionDecoder : Json.Decoder MousePosition
mousePositionDecoder =
  Json.map2 MousePosition
    ( Json.field "pageX" Json.int )
    ( Json.field "pageY" Json.int )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    RecordPosition position ->
      ( { model | position = Just position }, Cmd.none )
    DoDoubleClick ->
      ( { model | doubleClicks = model.doubleClicks + 1 }, Cmd.none )
    DoMouseDown ->
      ( { model | mouseDowns = model.mouseDowns + 1 }, Cmd.none )
    DoMouseUp ->
      ( { model | mouseUps = model.mouseUps + 1 }, Cmd.none )
    DoMouseEnter ->
      ( { model | mouseEnters = model.mouseEnters + 1 }, Cmd.none )
    DoMouseLeave ->
      ( { model | mouseLeaves = model.mouseLeaves + 1 }, Cmd.none )
    DoMouseOver ->
      ( { model | mouseOvers = model.mouseOvers + 1 }, Cmd.none )
    DoMouseOut ->
      ( { model | mouseOuts = model.mouseOuts + 1 }, Cmd.none )
