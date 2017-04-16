module Elmer.Component exposing
  ( Component
  , ViewFunction
  , UpdateFunction
  , LocationParserFunction
  , render
  )

import Elmer.Http.Internal exposing (HttpRequest)
import Elmer.Spy.Internal exposing (Spy)
import Navigation
import Html exposing (Html)

type alias ViewFunction model msg =
    model -> Html msg

type alias UpdateFunction model msg =
    msg -> model -> ( model, Cmd msg )

type alias LocationParserFunction msg =
    Navigation.Location -> msg

type alias Component model msg =
    { model : model
    , view : ViewFunction model msg
    , update : UpdateFunction model msg
    , targetSelector : Maybe String
    , locationParser : Maybe (LocationParserFunction msg)
    , location : Maybe String
    , httpRequests : List HttpRequest
    , deferredCommands : List (Cmd msg)
    , dummyCommands : List String
    , subscriptions : Sub msg
    , spies : List Spy
    }

render : Component model msg -> Html msg
render component =
  component.view component.model
