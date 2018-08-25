module Elmer.TestApps.ApplicationTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Url exposing (Url)
import Time
import Task

type Msg
  = OnUrlRequest UrlRequest
  | OnUrlChange Url
  | FunTaskResult String

type alias Model =
  { name : String }

init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
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

funCommand : (String -> Msg) -> String -> Cmd Msg
funCommand tagger message =
  Time.now
    |> Task.map Time.posixToMillis
    |> Task.map (\millis -> message ++ String.fromInt millis)
    |> Task.perform tagger