module Elmer.TestApps.EventPropagationTestApp exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as Json

type alias Model =
  { eventCount : Int }

type Msg
  = HandleBasicEvent
  | HandleInputEvent String
  | HandleBooleanEvent Bool

defaultModel : Model
defaultModel =
  { eventCount = 0 }

view : Model -> Html Msg
view model =
  Html.div (eventHandlers "root")
    [ Html.div (eventHandlers "parent")
      [ Html.div (eventHandlers "child")
        [ Html.div [ Attr.id "no-events" ] [ Html.text "Something" ]
        ]
      ]
    ]

viewWithNonPropagatingEvent : String -> Model -> Html Msg
viewWithNonPropagatingEvent eventName model =
  Html.div (eventHandlers "root")
    [ Html.div [ Attr.id "parent", nonPropagatingEvent eventName HandleBasicEvent ]
      [ Html.div (eventHandlers "child")
        [ Html.div [ Attr.id "no-events" ] [ Html.text "Something" ]
        ]
      ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleBasicEvent ->
      ( { model | eventCount = model.eventCount + 1 }, Cmd.none )
    HandleInputEvent _ ->
      ( { model | eventCount = model.eventCount + 1 }, Cmd.none )
    HandleBooleanEvent _ ->
      ( { model | eventCount = model.eventCount + 1 }, Cmd.none )

eventHandlers : String -> List (Attribute Msg)
eventHandlers name =
  [ Attr.id name
  , onClick HandleBasicEvent
  , onDoubleClick HandleBasicEvent
  , onMouseDown HandleBasicEvent
  , onMouseUp HandleBasicEvent
  , onMouseEnter HandleBasicEvent
  , onMouseLeave HandleBasicEvent
  , onMouseOver HandleBasicEvent
  , onMouseOut HandleBasicEvent
  , onFocus HandleBasicEvent
  , onBlur HandleBasicEvent
  , onInput HandleInputEvent
  , onCheck HandleBooleanEvent
  ]

nonPropagatingEvent : String -> Msg -> Attribute Msg
nonPropagatingEvent name msg =
  let
    options = { stopPropagation = True, preventDefault = False }
  in
    onWithOptions name options (Json.succeed msg)
