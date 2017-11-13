module Elmer.Context.Internal exposing
  ( Context
  , ViewFunction
  , UpdateFunction
  , LocationParserFunction
  , render
  , update
  )

import Html exposing (Html)
import Navigation
import Elmer.Spy.Internal exposing (Spy)
import Elmer.Http.Internal exposing (HttpRequest)

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
    , commandGenerator : Maybe (() -> Cmd msg)
    , messages : List msg
    }


render : Context model msg -> Html msg
render context =
  context.view context.model


update : msg -> Context model msg -> ( Context model msg, Cmd msg )
update message context =
  let
      ( updatedModel, command ) =
          context.update message context.model

      updatedContext =
          { context | model = updatedModel }
  in
      ( updatedContext, command )
