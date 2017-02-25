module Elmer.TestApps.MouseTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onDoubleClick, onMouseDown, onMouseUp)

type alias Model =
  { clicks : Int
  , doubleClicks : Int
  , mouseDowns : Int
  , mouseUps : Int
  }

type Msg
  = DoClick
  | DoDoubleClick
  | DoMouseDown
  | DoMouseUp

defaultModel : Model
defaultModel =
  { clicks = 0
  , doubleClicks = 0
  , mouseDowns = 0
  , mouseUps = 0
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div [ Attr.class "noEvents" ] []
    , Html.div
      [ Attr.class "button"
      , onClick DoClick
      , onDoubleClick DoDoubleClick
      , onMouseDown DoMouseDown
      , onMouseUp DoMouseUp
      ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((toString model.clicks) ++ " clicks!") ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    DoDoubleClick ->
      ( { model | doubleClicks = model.doubleClicks + 1 }, Cmd.none )
    DoMouseDown ->
      ( { model | mouseDowns = model.mouseDowns + 1 }, Cmd.none )
    DoMouseUp ->
      ( { model | mouseUps = model.mouseUps + 1 }, Cmd.none )
