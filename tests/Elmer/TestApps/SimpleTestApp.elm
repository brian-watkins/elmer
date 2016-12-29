module Elmer.TestApps.SimpleTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr

type alias Model =
  { name : String }

type Msg = Msg

defaultModel : Model
defaultModel =
  { name = "Cool Person" }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root", Attr.class "styled" ] [ Html.text "Some text" ]

viewWithChildren : Model -> Html Msg
viewWithChildren model =
  Html.div [ Attr.id "root", Attr.class "styled" ]
    [ Html.div [] [ Html.text "Some text" ]
    , Html.div [] [ Html.div [] [ Html.text "Child text" ] ]
    ]

textView : Model -> Html Msg
textView model =
  Html.text "Some text"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )
