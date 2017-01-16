module Elmer.Http.Stub exposing
  ( HttpStatus
  , httpStatus
  , get
  , post
  , withError
  , withStatus
  , withBody
  , deferResponse
  )

import Elmer.Http exposing (..)
import Http
import Dict

type alias HttpStatus =
  { code: Int
  , message: String
  }

httpStatus : Int -> String -> HttpStatus
httpStatus code message =
  { code = code
  , message = message
  }

get : String -> HttpResponseStub
get url =
  defaultResponse "GET" url

post : String -> HttpResponseStub
post url =
  defaultResponse "POST" url

defaultResponse : String -> String -> HttpResponseStub
defaultResponse method url =
  { url = url
  , method = method
  , response =
    HttpResponse { url = url
    , status = { code = 200, message = "Ok" }
    , headers = Dict.empty
    , body = ""
    }
  , deferResponse = False
  }

withError : Http.Error -> HttpResponseStub -> HttpResponseStub
withError error responseStub =
  { responseStub | response = HttpError error }

withStatus : HttpStatus -> HttpResponseStub -> HttpResponseStub
withStatus newStatus =
  mapResponse (\r -> { r | status = newStatus })

withBody : String -> HttpResponseStub -> HttpResponseStub
withBody newBody =
  mapResponse (\r -> { r | body = newBody })

deferResponse : HttpResponseStub -> HttpResponseStub
deferResponse response =
  { response | deferResponse = True }

mapResponse : (Http.Response String -> Http.Response String) -> HttpResponseStub -> HttpResponseStub
mapResponse mapper responseStub =
  case responseStub.response of
    HttpResponse response ->
      let
        updatedResponse = mapper response
      in
        { responseStub | response = HttpResponse updatedResponse }
    HttpError error ->
      responseStub
