module Elmer.TestApps.DocumentTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Browser exposing (Document)


type Msg
  = Msg

type alias Model =
  { name : String }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { name = "Cool Dude" }, Cmd.none )

view : Model -> Document Msg
view model =
  { title = "Fun Title"
  , body = [ pageView model ]
  }

pageView : Model -> Html Msg
pageView model =
  Html.div []
  [ Html.div [ Attr.id "some-element" ]
    [ Html.text "Fun Stuff" ]
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
