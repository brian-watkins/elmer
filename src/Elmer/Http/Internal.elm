module Elmer.Http.Internal exposing
  ( HttpHeader
  , HttpRequestData
  , HttpRequestFunction
  , HttpRequest
  , HttpStub
  , HttpResponseStub(..)
  , HttpResponseResult(..)
  , HttpStatus(..)
  , asHttpRequest
  , route
  )

import Http

type alias HttpRequestData =
  { method: String
  , url: String
  , body: Maybe String
  , headers: List HttpHeader
  }

type alias HttpRequestFunction a b =
  (Result Http.Error a -> b) -> Http.Request a -> Cmd b

type alias HttpHeader =
  { name: String
  , value: String
  }

type alias HttpRequest a =
  { method: String
  , url: String
  , headers: List HttpHeader
  , body: Maybe String
  , responseHandler: (Http.Response String -> Result String a)
  }

type HttpResponseStub
  = HttpResponseStub HttpStub

type alias HttpStub =
  { url: String
  , method: String
  , response: HttpResponseResult
  , deferResponse: Bool
  }

type HttpResponseResult
  = Response (Http.Response String)
  | Error Http.Error


type HttpStatus
  = HttpStatus Status

type alias Status =
  { code: Int
  , message: String
  }


asHttpRequest : Http.Request a -> HttpRequest a
asHttpRequest request =
  Native.Http.asHttpRequest request


route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""
