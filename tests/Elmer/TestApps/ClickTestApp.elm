module Elmer.TestApps.ClickTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onDoubleClick)

type alias Model =
  { clicks : Int
  , doubleClicks : Int
  }

type Msg
  = DoClick
  | DoDoubleClick

defaultModel : Model
defaultModel =
  { clicks = 0
  , doubleClicks = 0
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div [ Attr.class "noEvents" ] []
    , Html.div [ Attr.class "button", onClick DoClick, onDoubleClick DoDoubleClick ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((toString model.clicks) ++ " clicks!") ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    DoDoubleClick ->
      ( { model | doubleClicks = model.doubleClicks + 1 }, Cmd.none )
