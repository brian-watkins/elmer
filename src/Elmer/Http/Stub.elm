module Elmer.Http.Stub exposing
  ( get
  , post
  , delete
  , withError
  , withStatus
  , withBody
  , deferResponse
  )

{-| Functions for building stubbed responses to Http requests.

# Specify the Method and Route
@docs get, post, delete

# Describe the Response body
@docs withBody

# Describe the Response Status
@docs withStatus, withError

# Defer the Response
@docs deferResponse

-}

import Elmer.Http
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Status as Status
import Http
import Dict


{-| Stub the response to a GET request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
get : String -> Elmer.Http.HttpResponseStub
get url =
  defaultResponse "GET" url


{-| Stub the response to a POST request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
post : String -> Elmer.Http.HttpResponseStub
post url =
  defaultResponse "POST" url


{-| Stub the response to a DELETE request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
delete : String -> Elmer.Http.HttpResponseStub
delete url =
  defaultResponse "DELETE" url


defaultResponse : String -> String -> HttpResponseStub
defaultResponse method url =
  let
    (HttpStatus okStatus) = Status.ok
  in
    HttpResponseStub
      { url = url
      , method = method
      , response =
        Response { url = url
        , status = okStatus
        , headers = Dict.empty
        , body = ""
        }
      , deferResponse = False
      }


{-| Build a response stub that results in an `Http.Error`.

Suppose you want to describe the behavior that should result when a request
times out. You could create a stubbed response like so:

    get "http://fake.com/fake"
      |> withError Http.Error.Timout

-}
withError : Http.Error -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withError error (HttpResponseStub stub) =
  HttpResponseStub { stub | response = Error error }


{-| Build a response stub that returns some particular status.

Suppose you want to describe the behavior that should result when a request
returns a `500 Internal Server Error`. You could create a stubbed response like so:

    get "http://fake.com/fake"
      |> withStatus Elmer.Http.Status.serverError

-}
withStatus : HttpStatus -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withStatus (HttpStatus newStatus) =
  mapResponse (\r -> { r | status = newStatus })


{-| Build a response stub that returns the specified string as its body.

Suppose you want to describe the behavior that results when a response body is
parsed. You could create a stub like so:

    post "http://fake.com/fake"
      |> withBody "{\"name\":\"Fun Person\"}"

-}
withBody : String -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withBody newBody =
  mapResponse (\r -> { r | body = newBody })


{-| Defer a response.

The response will not be processed until `Elmer.Command.resolveDeferred` is called.
-}
deferResponse : Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
deferResponse (HttpResponseStub stub) =
  HttpResponseStub { stub | deferResponse = True }

mapResponse : (Http.Response String -> Http.Response String) -> HttpResponseStub -> HttpResponseStub
mapResponse mapper (HttpResponseStub stub) =
  case stub.response of
    Response response ->
      let
        updatedResponse = mapper response
      in
        HttpResponseStub { stub | response = Response updatedResponse }
    Error error ->
      HttpResponseStub stub
