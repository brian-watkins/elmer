module Elmer.Http.Stub exposing
  ( for
  , withError
  , withStatus
  , withBody
  , withResult
  , withHeader
  , deferResponse
  )

{-| Functions for building stubbed responses to Http requests.

# Create a stubbed response
@docs for

# Describe the Response
@docs withBody, withHeader, withStatus, withError

# Provide a Result based on the Request
@docs withResult

# Defer the Response
@docs deferResponse

-}

import Elmer.Http exposing (HttpResponseStub)
import Elmer.Http.Internal as Internal
import Elmer.Http.Status as Status exposing (HttpStatus)
import Elmer.Http.Result as HttpResult exposing (HttpResult)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Http.Route exposing (HttpRoute)
import Http
import Dict

{-| Stub the response for a request to the specified route.

By default, this response will return an empty body with a status of
`200 OK`.
-}
for : HttpRoute -> HttpResponseStub
for route =
  defaultResponseStub route.method route.url


defaultResponseStub : String -> String -> HttpResponseStub
defaultResponseStub method url =
  Internal.HttpResponseStub
    { url = url
    , method = method
    , resultBuilder = (\_ -> defaultResult url)
    , deferResponse = False
    }

defaultResult : String -> HttpResult
defaultResult url =
  let
    (Internal.HttpStatus okStatus) = Status.ok
  in
    Internal.Response
      { url = url
      , status = okStatus
      , headers = Dict.empty
      , body = ""
      }

{-| Build a response stub that results in an `Http.Error`.

Suppose you want to describe the behavior that should result when a request
times out. You could create a stubbed response like so:

    Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fake.com/fake")
      |> withError Http.Error.Timout

-}
withError : Http.Error -> HttpResponseStub -> HttpResponseStub
withError error =
  withResult (\_ _ ->
    Internal.Error error
  )


{-| Build a response stub that returns some particular status.

Suppose you want to describe the behavior that should result when a request
returns a `500 Internal Server Error`. You could create a stubbed response like so:

    Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fake.com/fake")
      |> withStatus Elmer.Http.Status.serverError

-}
withStatus : HttpStatus -> HttpResponseStub -> HttpResponseStub
withStatus status =
  withResult (\_ ->
    HttpResult.withStatus status
  )


{-| Build a response stub that returns the specified string as its body.

Suppose you want to describe the behavior that results when a response body is
parsed. You could create a stub like so:

    Elmer.Http.Stub.for (Elmer.Http.Route.post "http://fake.com/fake")
      |> withBody "{\"name\":\"Fun Person\"}"

-}
withBody : String -> HttpResponseStub -> HttpResponseStub
withBody newBody =
  withResult (\_ ->
    HttpResult.withBody newBody
  )


{-| Build a response stub that has the specified header.

Add as many headers as necessary like so:

    Elmer.Http.Stub.for (Elmer.Http.Route.post "http://fake.com/fake")
      |> withBody "{\"name\":\"Fun Person\"}"
      |> withHeader ("x-fun-header", "something fun")
      |> withHeader ("x-awesome-header", "something awesome")
-}
withHeader : (String, String) -> HttpResponseStub -> HttpResponseStub
withHeader header =
  withResult (\_ ->
    HttpResult.withHeader header
  )


{-| Build a response stub that generates an HttpResult based on the matching
HttpRequest.

You could create a stub that returns a body only given a certain query string like so:

    Elmer.Http.Stub.for (Elmer.Http.Route.post "http://fake.com/fake")
      |> withResult (\request ->
        let
          queryString = Elmer.Http.Request.queryString request
        in
          if String.contains "funSport=bowling" queryString then
            Elmer.Http.Result.withBody "{\"sport\":\"bowling\"}"
          else
            Elmer.Http.Result.withStatus Elmer.Http.Status.notFound
      )

-}
withResult : (HttpRequest -> HttpResult -> HttpResult) -> HttpResponseStub -> HttpResponseStub
withResult builder (Internal.HttpResponseStub stub) =
  let
    composedBuilder =
      (\request ->
        stub.resultBuilder request
          |> builder request
      )
  in
    Internal.HttpResponseStub { stub | resultBuilder = composedBuilder }


{-| Defer a response.

The response will not be processed until `Elmer.Platform.Command.resolveDeferred` is called.

Note: If this stub is processed as part of a request made with `Http.toTask` then the
entire Task chain will be deferred until `Elmer.Platform.Command.resolveDeferred` is called.

-}
deferResponse : HttpResponseStub -> HttpResponseStub
deferResponse (Internal.HttpResponseStub stub) =
  Internal.HttpResponseStub { stub | deferResponse = True }
