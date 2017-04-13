module Elmer.Http.Stub exposing
  ( get
  , post
  , delete
  , put
  , patch
  , withError
  , withStatus
  , withBody
  , withResult
  , deferResponse
  )

{-| Functions for building stubbed responses to Http requests.

# Specify the Method and Route
@docs get, post, put, patch, delete

# Describe the Response body
@docs withBody

# Describe the Response Status
@docs withStatus, withError

# Provide a Result based on the Request
@docs withResult

# Defer the Response
@docs deferResponse

-}

import Elmer.Http
import Elmer.Http.Internal exposing (..)
import Elmer.Http.Status as Status
import Elmer.Http.Result as HttpResult
import Http
import Dict


{-| Stub the response to a GET request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
get : String -> Elmer.Http.HttpResponseStub
get url =
  defaultResponseStub "GET" url


{-| Stub the response to a POST request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
post : String -> Elmer.Http.HttpResponseStub
post url =
  defaultResponseStub "POST" url


{-| Stub the response to a DELETE request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
delete : String -> Elmer.Http.HttpResponseStub
delete url =
  defaultResponseStub "DELETE" url

{-| Stub the response to a PUT request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
put : String -> Elmer.Http.HttpResponseStub
put url =
  defaultResponseStub "PUT" url

{-| Stub the response to a PATCH request at the specified route.

By default, this response will return an empty body with a status of
`200 OK`.

Note: The route should not contain a query string.
-}
patch : String -> Elmer.Http.HttpResponseStub
patch url =
  defaultResponseStub "PATCH" url

defaultResponseStub : String -> String -> HttpResponseStub
defaultResponseStub method url =
  HttpResponseStub
    { url = url
    , method = method
    , resultBuilder = (\_ -> defaultResult url)
    , deferResponse = False
    }

defaultResult : String -> HttpResult
defaultResult url =
  let
    (HttpStatus okStatus) = Status.ok
  in
    Response
      { url = url
      , status = okStatus
      , headers = Dict.empty
      , body = ""
      }

{-| Build a response stub that results in an `Http.Error`.

Suppose you want to describe the behavior that should result when a request
times out. You could create a stubbed response like so:

    get "http://fake.com/fake"
      |> withError Http.Error.Timout

-}
withError : Http.Error -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withError error =
  withResult (\_ _ ->
      Error error
    )


{-| Build a response stub that returns some particular status.

Suppose you want to describe the behavior that should result when a request
returns a `500 Internal Server Error`. You could create a stubbed response like so:

    get "http://fake.com/fake"
      |> withStatus Elmer.Http.Status.serverError

-}
withStatus : HttpStatus -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withStatus (HttpStatus newStatus) =
  withResult (\_ result ->
      case result of
        Response response ->
          Response { response | status = newStatus }
        Error _ ->
          result
    )


{-| Build a response stub that returns the specified string as its body.

Suppose you want to describe the behavior that results when a response body is
parsed. You could create a stub like so:

    post "http://fake.com/fake"
      |> withBody "{\"name\":\"Fun Person\"}"

-}
withBody : String -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withBody newBody =
  withResult (\_ result ->
      HttpResult.withBody newBody result
    )


{-| Build a response stub that generates an HttpResult based on the matching
HttpRequest.

You could create a stub that echoes back the posted body like so:

    post "http://fake.com/fake"
      |> withResult (\request result ->
        Elmer.Http.Result.body (Elmer.Http.Request.body request) result
      )

-}
withResult : (HttpRequest -> HttpResult -> HttpResult) -> Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
withResult builder (HttpResponseStub stub) =
  let
    composedBuilder =
      (\request ->
        stub.resultBuilder request
          |> builder request
      )
  in
    HttpResponseStub { stub | resultBuilder = composedBuilder }


{-| Defer a response.

The response will not be processed until `Elmer.Platform.Command.resolveDeferred` is called.
-}
deferResponse : Elmer.Http.HttpResponseStub -> Elmer.Http.HttpResponseStub
deferResponse (HttpResponseStub stub) =
  HttpResponseStub { stub | deferResponse = True }
