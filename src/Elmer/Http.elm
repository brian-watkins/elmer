module Elmer.Http exposing
  ( HttpResponseStub
  , expectPOST
  , expectGET
  , expectDELETE
  , expectPUT
  , expectPATCH
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
@docs expectGET, expectPOST, expectPUT, expectPATCH, expectDELETE, clearRequestHistory

-}

import Http
import Dict
import Elmer exposing (Matcher)
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Server as Server
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform
import Elmer.Printer exposing (..)
import Expect exposing (Expectation)


{-| Represents a stubbed HttpResponse

Use `Elmer.Http.Stub` to build an `HttpResponseStub`.
-}
type alias HttpResponseStub
  = HttpInternal.HttpResponseStub

{-| Override `Http.send` and register HttpResponseStubs to be returned
when the appropriate request is received. Used in conjunction with
`Elmer.Spy.use`.

Suppose you have a component that requests information about a user when
a button is clicked. You could register a stub for that request like so

    let
      stubbedResponse = Elmer.Http.Stub.post "http://fun.com/user"
        |> Elmer.Http.Stub.withBody
          "{\"name\":\"Super User\",\"type\":\"admin\"}"
    in
      componentState
        |> Spy.use [ serve [ stubbedResponse ] ]
        |> Markup.target "#request-data-button"
        |> Elmer.Html.Event.click
        |> Markup.target "#data-result"
        |> Markup.expect (Matchers.element <| Matchers.hasText "Hello, Super User!")

-}
serve : List HttpResponseStub -> Spy
serve responseStubs =
  Spy.create "Http.send" (\_ -> Http.send)
    |> andCallFake (Server.stubbedSend responseStubs)


{-| Override `Http.send` and record requests as they are received.
Used in conjunction with `Elmer.Spy.use`.

Suppose you simply want to make an expectation about a request without
describing the behavior that results when its response is received.

    componentState
      |> Spy.use [ spy ]
      |> Markup.target "#request-data-button"
      |> Elmer.Http.Event.click
      |> expectGET "http://fun.com/user" Elmer.Http.Matchers.wasSent

-}
spy : Spy
spy =
  Spy.create "Http.send" (\_ -> Http.send)
    |> andCallFake Server.dummySend


{-| Clear any Http requests that may have been recorded at an earlier point
in the history of this ComponentState.
-}
clearRequestHistory : Elmer.ComponentState model msg -> Elmer.ComponentState model msg
clearRequestHistory =
  ComponentState.map (\component ->
    if List.isEmpty component.httpRequests then
      ComponentState.failure "No HTTP requests to clear"
    else
      ComponentState.with { component | httpRequests = [] }
  )


{-| Expect a matching POST request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectPOST : String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectPOST =
  expectRequest "POST"

{-| Expect a matching GET request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectGET : String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectGET =
  expectRequest "GET"

{-| Expect a matching DELETE request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectDELETE : String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectDELETE =
  expectRequest "DELETE"

{-| Expect a matching PUT request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectPUT : String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectPUT =
  expectRequest "PUT"

{-| Expect a matching PATCH request to the specified url.

The path argument is just the url you expect (relative or absolute) up to but not
including the query string. See `Elmer.Http.Matchers` for request matchers.

Note: This requires the use of `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectPATCH : String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectPATCH =
  expectRequest "PATCH"

expectRequest : String -> String -> Matcher HttpRequest -> Matcher (Elmer.ComponentState model msg)
expectRequest method url requestMatcher =
  ComponentState.mapToExpectation <|
    \component ->
      if String.contains "?" url then
        Expect.fail <| format
          [ message "The expected route contains a query string" url
          , description "Use the hasQueryParam matcher instead"
          ]
      else
        case hasRequest component.httpRequests method url of
          Just request ->
            requestMatcher request
          Nothing ->
            if List.isEmpty component.httpRequests then
              Expect.fail <| format
                [ message "Expected request for" (method ++ " " ++ url)
                , description "but no requests have been made"
                ]
            else
              let
                requests = String.join "\n" (List.map (\r -> r.method ++ " " ++ r.url) component.httpRequests)
              in
                Expect.fail <| format
                  [ message "Expected request for" (method ++ " " ++ url)
                  , message "but only found these requests" requests
                  ]


hasRequest : List HttpRequest -> String -> String -> Maybe HttpRequest
hasRequest requests method url =
  List.filter (\r -> r.method == method && (HttpInternal.route r.url) == url) requests
    |> List.head
