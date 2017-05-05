module Elmer.Http.Internal exposing
  ( HttpHeader
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
asHttpRequestHandler request =
  Native.Http.asHttpRequestHandler request


route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""
