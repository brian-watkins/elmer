module Elmer.Http exposing
  ( HttpResponseStub
  , expectPOST
  , expectGET
  , expectDELETE
  , clearRequestHistory
  , serve
  , spy
  )

{-| Functions for handling Http requests in your tests.

Your component makes Http requests. You want to describe the behavior of your
component. What to do?

1. Create an HttpResponseStub -- see `Elmer.Http.Stub`

2. Serve it up during your test

3. Smile!

# Serve Stubbed Responses
@docs HttpResponseStub, serve, spy

# Make Expectations about Http Requests
@docs expectGET, expectPOST, expectDELETE, clearRequestHistory

-}

import Http
import Dict
import Elmer exposing (Matcher)
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Server as Server
import Elmer.Internal as Internal exposing (..)
import Elmer.Platform.Command as Command
import Elmer.Platform as Platform exposing (PlatformOverride)
import Elmer.Command.Internal as InternalCommand
import Elmer.Printer exposing (..)
import Expect exposing (Expectation)


{-| Represents a stubbed HttpResponse

Use `Elmer.Http.Stub` to build an `HttpResponseStub`.
-}
type alias HttpResponseStub
  = HttpInternal.HttpResponseStub


{-| Override `Http.send` and register HttpResponseStubs to be returned
when the appropriate request is received. Used in conjunction with
`Elmer.Command.use`.

Suppose you have a component that requests information about a user when
a button is clicked. You could register a stub for that request like so

    let
      stubbedResponse = Elmer.Http.Stub.post "http://fun.com/user"
        |> Elmer.Http.Stub.withBody
          "{\"name\":\"Super User\",\"type\":\"admin\"}"
    in
      componentState
        |> Markup.find "#request-data-button"
        |> Elmer.Command.use [ serve [ stubbedResponse ] ] click
        |> Markup.find "#data-result"
        |> Markup.expectElement (Matchers.hasText "Hello, Super User!")

-}
serve : List HttpResponseStub -> PlatformOverride
serve responseStubs =
  Command.override (\_ -> Http.send) (Server.stubbedSend responseStubs)

{-| Override `Http.send` and record requests as they are received.
Used in conjunction with `Elmer.Command.use`.

Suppose you simply want to make an expectation about a request without
describing the behavior that results when its response is received.

    componentState
      |> Markup.find "#request-data-button"
      |> Elmer.Command.use [ spy ] click
      |> expectGET "http://fun.com/user" Elmer.Http.Matchers.hasBeenRequested

-}
spy : PlatformOverride
spy =
  Command.override (\_ -> Http.send) Server.dummySend


{-| Clear any Http requests that may have been recorded at an earlier point
in the history of this ComponentState.
-}
clearRequestHistory : ComponentState model msg -> ComponentState model msg
clearRequestHistory =
  Internal.map (\componentState ->
    if List.isEmpty componentState.httpRequests then
      Failed "No HTTP requests to clear"
    else
      Ready { componentState | httpRequests = [] }
  )


{-| Expect a matching POST request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectPOST : String -> Matcher HttpRequestData -> Matcher (ComponentState model msg)
expectPOST =
  expectRequest "POST"

{-| Expect a matching GET request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectGET : String -> Matcher HttpRequestData -> Matcher (ComponentState model msg)
expectGET =
  expectRequest "GET"

{-| Expect a matching DELETE request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectDELETE : String -> Matcher HttpRequestData -> Matcher (ComponentState model msg)
expectDELETE =
  expectRequest "DELETE"


expectRequest : String -> String -> Matcher HttpRequestData -> Matcher (ComponentState model msg)
expectRequest method url requestMatcher =
  Internal.mapToExpectation <|
    \componentState ->
      if String.contains "?" url then
        Expect.fail <| format
          [ message "The expected route contains a query string" url
          , description "Use the hasQueryParam matcher instead"
          ]
      else
        case hasRequest componentState.httpRequests method url of
          Just request ->
            requestMatcher request
          Nothing ->
            if List.isEmpty componentState.httpRequests then
              Expect.fail <| format
                [ message "Expected request for" (method ++ " " ++ url)
                , description "but no requests have been made"
                ]
            else
              let
                requests = String.join "\n\n\t" (List.map (\r -> r.method ++ " " ++ r.url) componentState.httpRequests)
              in
                Expect.fail <| format
                  [ message "Expected request for" (method ++ " " ++ url)
                  , message "but only found these requests" requests
                  ]


hasRequest : List HttpRequestData -> String -> String -> Maybe HttpRequestData
hasRequest requests method url =
  List.filter (\r -> r.method == method && (HttpInternal.route r.url) == url) requests
    |> List.head
