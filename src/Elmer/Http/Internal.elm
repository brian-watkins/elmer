module Elmer.Http.Internal exposing
  ( HttpState(..)
  , HttpHeader
  , HttpRequestFunction
  , HttpRequestHandler
  , HttpRequest
  , HttpStub
  , HttpResponseStub(..)
  , HttpResult(..)
  , HttpStatus(..)
  , HttpRoute
  , asHttpRequestHandler
  , route
  )

import Http
import Json.Decode as Json
import Elmer.Value as Value

type HttpState
  = Requests

type alias HttpRoute =
  { method : String
  , url : String
  }

type alias HttpRequest =
  { method: String
  , url: String
  , headers: List HttpHeader
  , body: Maybe String
  }

type alias HttpRequestFunction a b =
  (Result Http.Error a -> b) -> Http.Request a -> Cmd b

type alias HttpHeader =
  { name: String
  , value: String
  }

type alias HttpStringBody =
  { mimeType: String
  , body: String
  }

type alias HttpRequestHandler a =
  { request: HttpRequest
  , responseHandler: (Http.Response String -> Result String a)
  }

type HttpResponseStub
  = HttpResponseStub HttpStub

type alias HttpStub =
  { url: String
  , method: String
  , resultBuilder : (HttpRequest -> HttpResult)
  , deferResponse: Bool
  }

type HttpResult
  = Response (Http.Response String)
  | Error Http.Error


type HttpStatus
  = HttpStatus Status

type alias Status =
  { code: Int
  , message: String
  }


asHttpRequestHandler : Http.Request a -> HttpRequestHandler a
asHttpRequestHandler httpRequest =
  { request = makeHttpRequest httpRequest
  , responseHandler =
      case Value.mapArg (Value.decode expectDecoder) httpRequest of
        Ok handler ->
          handler
        Err err ->
          Debug.crash <| "Error fetching" ++ err
  }

expectDecoder : Json.Decoder Json.Value
expectDecoder =
  Json.at ["expect", "responseToResult"] Json.value


makeHttpRequest : Http.Request a -> HttpRequest
makeHttpRequest =
  Value.mapArg <|
    \r ->
      { method = Value.field "method" r
      , url = Value.field "url" r
      , headers = List.map makeHttpHeader <| Value.field "headers" r
      , body = makeBody r
      }


makeBody : a -> Maybe String
makeBody request =
  let
    body = Value.field "body" request
  in
    if Value.constructor body == "StringBody" then
      Value.mapArg2 HttpStringBody body
        |> .body
        |> Just
    else
      Nothing


makeHttpHeader : Http.Header -> HttpHeader
makeHttpHeader header =
  Value.mapArg2 HttpHeader header

route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""
