module Elmer.Internal exposing
  ( Component
  , ComponentState(..)
  , ViewFunction
  , UpdateFunction
  , LocationParserFunction
  , map
  , mapToExpectation
  )

import Elmer.Http.Internal exposing (HttpRequestData)
import Elmer.Html.Types exposing (HtmlElement)
import Html exposing (Html)
import Navigation
import Expect

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
    , targetElement : Maybe (HtmlElement msg)
    , locationParser : Maybe (LocationParserFunction msg)
    , location : Maybe String
    , httpRequests : List HttpRequestData
    , deferredCommands : List (Cmd msg)
    , dummyCommands : List String
    , subscriptions : Sub msg
    }


type ComponentState model msg
    = Ready (Component model msg)
    | Failed String


map : (Component model msg -> ComponentState model msg) -> ComponentState model msg -> ComponentState model msg
map mapper componentStateResult =
    case componentStateResult of
        Ready componentState ->
            mapper componentState

        Failed message ->
            Failed message


mapToExpectation : (Component model msg -> Expect.Expectation) -> ComponentState model msg -> Expect.Expectation
mapToExpectation mapper componentStateResult =
  case componentStateResult of
    Ready componentState ->
      mapper componentState
    Failed message ->
      Expect.fail message
