module Elmer.Context exposing
  ( Context
  , ViewFunction
  , UpdateFunction
  , LocationParserFunction
  , render
  , defaultContext
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

type alias Context model msg =
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


defaultContext : model -> ViewFunction model msg -> UpdateFunction model msg -> Context model msg
defaultContext model view update =
  { model = model
  , view = view
  , update = update
  , targetSelector = Nothing
  , locationParser = Nothing
  , location = Nothing
  , httpRequests = []
  , deferredCommands = []
  , dummyCommands = []
  , subscriptions = Sub.none
  , spies = []
  }


render : Context model msg -> Html msg
render context =
  context.view context.model
