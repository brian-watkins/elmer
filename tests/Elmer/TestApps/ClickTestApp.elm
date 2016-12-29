module Elmer.TestApps.ClickTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

type alias Model =
  { clicks : Int }

type Msg
  = DoClick

defaultModel : Model
defaultModel =
  { clicks = 0 }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div [ Attr.class "noEvents" ] []
    , Html.div [ Attr.class "button", onClick DoClick ] [ Html.text "Click me!" ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
