module Elmer.Http exposing
  ( HttpResponseStub
  , expect
  , expectThat
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
@docs expect, expectThat, clearRequestHistory

-}

import Http
import Dict
import Elmer exposing (Matcher)
import Elmer.Http.Internal as Http_ exposing (..)
import Elmer.Http.Server as Server
import Elmer.Http.Route as Route
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform
import Elmer.Printer exposing (..)
import Expect exposing (Expectation)
import Test.Runner


{-| Represents a stubbed HttpResponse

Use `Elmer.Http.Stub` to build an `HttpResponseStub`.
-}
type alias HttpResponseStub
  = Http_.HttpResponseStub

{-| Override `Http.send` and register HttpResponseStubs to be returned
when the appropriate request is received. Used in conjunction with
`Elmer.Spy.use`.

Suppose you have a component that requests information about a user when
a button is clicked. You could register a stub for that request like so

    let
      stubbedResponse = Elmer.Http.Stub.for (Elmer.Http.Route.post "http://fun.com/user")
        |> Elmer.Http.Stub.withBody
          "{\"name\":\"Super User\",\"type\":\"admin\"}"
    in
      testState
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

    testState
      |> Spy.use [ spy ]
      |> Markup.target "#request-data-button"
      |> Elmer.Http.Event.click
      |> Elmer.Http.expect (Elmer.Http.Route.get "http://fun.com/user")

-}
spy : Spy
spy =
  Spy.create "Http.send" (\_ -> Http.send)
    |> andCallFake Server.dummySend


{-| Clear any Http requests that may have been recorded at an earlier point
in the history of this TestState.
-}
clearRequestHistory : Elmer.TestState model msg -> Elmer.TestState model msg
clearRequestHistory =
  TestState.map (\context ->
    if List.isEmpty context.httpRequests then
      TestState.failure "No HTTP requests to clear"
    else
      TestState.with { context | httpRequests = [] }
  )


{-| Expect one or more requests to the specified route.

    expect (Elmer.Http.Route.get "http://fun.com/fun.html")

If no requests have been made to the specified route, the test will fail.

Note: This must be used in conjunction with `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expect : HttpRoute -> Matcher (Elmer.TestState model msg)
expect route =
  TestState.mapToExpectation <|
    \context ->
      if List.isEmpty context.httpRequests then
        Expect.fail <| format
          [ message "Expected request for" (route.method ++ " " ++ route.url)
          , description "but no requests have been made"
          ]
      else
        case hasRequest context.httpRequests route.method route.url of
          Just _ ->
            Expect.pass
          Nothing ->
            let
              requests = String.join "\n" (List.map (\r -> r.method ++ " " ++ r.url) context.httpRequests)
            in
              Expect.fail <| format
                [ message "Expected request for" (route.method ++ " " ++ route.url)
                , message "but only found these requests" requests
                ]

{-| Make some expectation about requests to the specified route.

    expectThat (Elmer.Http.Route.get "http://fun.com/fun") (
      Elmer.each <| Elmer.Http.Matchers.hasHeader ("X-Auth-Token", "MY-TOKEN")
    )

If no requests have been made to the specified route, an empty list
will be passed to the `Matcher (List HttpRequest)`.

Note: This must be used in conjunction with `Elmer.Http.serve` or `Elmer.Http.spy`.
-}
expectThat : HttpRoute -> Matcher (List HttpRequest) -> Matcher (Elmer.TestState model msg)
expectThat route matcher =
  TestState.mapToExpectation <|
    \context ->
      let
        result =
          List.filter (matchesRequest route.method route.url) context.httpRequests
            |> matcher
      in
        case Test.Runner.getFailureReason result of
          Just failure ->
            Expect.fail <| format
              [ message "Requests matching" (route.method ++ " " ++ route.url)
              , description "failed to meet the expectations:"
              , description <| formatFailure failure
              ]
          Nothing ->
            Expect.pass

hasRequest : List HttpRequest -> String -> String -> Maybe HttpRequest
hasRequest requests method url =
  List.filter (matchesRequest method url) requests
    |> List.head

matchesRequest : String -> String -> HttpRequest -> Bool
matchesRequest method url request =
  request.method == method && (Http_.route request.url) == url
