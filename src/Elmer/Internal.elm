module Elmer.Internal exposing
  ( Component
  , ComponentState(..)
  , ViewFunction
  , UpdateFunction
  , LocationParserFunction
  , map
  , mapToExpectation
  , boolToString
  )

import Elmer.Http.Internal exposing (HttpRequest)
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
    , targetSelector : Maybe String
    , locationParser : Maybe (LocationParserFunction msg)
    , location : Maybe String
    , httpRequests : List HttpRequest
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
        |> clearSpies
    Failed message ->
      Expect.fail message


clearSpies : Expect.Expectation -> Expect.Expectation
clearSpies expectation =
  if Native.Spy.clearSpies () then
    expectation
  else
    Expect.fail "Failed to clear spies! (This should never happen)"


boolToString : Bool -> String
boolToString bool =
  Basics.toString bool
    |> String.toLower
