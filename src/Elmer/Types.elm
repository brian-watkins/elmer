module Elmer.Types exposing (..)

import Html exposing (Html)
import Navigation
import Json.Decode as Json

type HtmlElement msg
    = Node (HtmlNode msg)
    | Text String


type alias HtmlNode msg =
    { tag : String
    , facts : String
    , children : List (HtmlElement msg)
    , events : List (HtmlEvent msg)
    }


type alias HtmlEvent msg =
    { eventType : String
    , decoder : Json.Decoder msg
    }


type alias ViewFunction model msg =
    model -> Html msg


type alias UpdateFunction model msg =
    msg -> model -> ( model, Cmd msg )


type alias LocationParserFunction msg =
    Navigation.Location -> msg

type alias HttpRequestData =
  { method: String
  , url: String
  , body: Maybe String
  }

type alias HtmlComponentState model msg =
    { model : model
    , view : ViewFunction model msg
    , update : UpdateFunction model msg
    , targetNode : Maybe (HtmlNode msg)
    , locationParser : Maybe (LocationParserFunction msg)
    , location : Maybe String
    , httpRequests : List HttpRequestData
    }


type ComponentStateResult model msg
    = CurrentState (HtmlComponentState model msg)
    | UpstreamFailure String
