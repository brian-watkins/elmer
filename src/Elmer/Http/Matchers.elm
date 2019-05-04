module Elmer.Http.Matchers exposing
  ( wasRequested
  , hasBody
  , hasQueryParam
  , hasHeader
  )

{-| Make expectations about Http requests sent by the component under test.

These matchers should be used with `Elmer.Http.expect`.

Note: Your test must use `Elmer.Http.serve` or `Elmer.Http.spy` at the
appropriate time to allow Elmer to record the requests sent by the component
under test.

# Matchers
@docs wasRequested, hasBody, hasQueryParam, hasHeader
-}

import Expect
import Http
import Url.Builder as Builder
import Elmer exposing (Matcher)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Http.Internal as Http_
import Elmer.Message exposing (..)


{-| Expect that exactly some number of requests have been recorded.

    Elmer.Http.expect (Elmer.Http.Route.get "http://fun.com/fun.html") (
      wasRequested 3
    )
-}
wasRequested : Int -> Matcher (List HttpRequest)
wasRequested times =
  \requests ->
    if List.length requests == times then
      Expect.pass
    else
      Expect.fail <|
        "Expected "
          ++ (String.fromInt times)
          ++ " "
          ++ (pluralize "request" times)
          ++ ", but recorded "
          ++ (String.fromInt <| List.length requests)
          ++ " "
          ++ (pluralize "request" (List.length requests))

pluralize : String -> Int -> String
pluralize word num =
  if num > 1 || num == 0 then
    word ++ "s"
  else
    word

{-| Match a request with the specified body.

    Elmer.Http.expect (Elmer.Http.Route.post "http://fake.com/fake") (
      Elmer.some <| 
        hasBody "{\"name\":\"Fun Person\"}"
    )

-}
hasBody : String -> Matcher HttpRequest
hasBody expectedBody =
  \request ->
    case request.body of
      Just body ->
        if body == expectedBody then
          Expect.pass
        else
          Expect.fail (format [ fact "Expected request to have body" expectedBody, fact "but it has" body ])
      Nothing ->
        Expect.fail (format [ fact "Expected request to have body" expectedBody, note "but it has no body" ])


{-| Match a request that has a query string containing the specified name and value.

Note: You don't need to worry about url encoding the name or value.

    Elmer.Http.expect (Elmer.Http.Route.get "http://fake.com/fake") (
      Elmer.some <| 
        hasQueryParam ( "name", "Fun Person" )
    )

-}
hasQueryParam : ( String, String ) -> Matcher HttpRequest
hasQueryParam ( key, value ) =
  \request ->
    let
      query = queryString request
      params = String.split "&" query
      expectedParam = Builder.toQuery [ Builder.string key value ]
        |> String.dropLeft 1
    in
      if String.isEmpty query then
        Expect.fail (format [ fact "Expected request to have query param" ( key ++ " = " ++ value), note "but it has no query string" ])
      else if List.member expectedParam params then
        Expect.pass
      else
        Expect.fail (format [ fact "Expected request to have query param" ( key ++ " = " ++ value), fact "but it has" query ])

queryString : HttpRequest -> String
queryString request =
  Http_.queryString request.url
    |> Maybe.withDefault ""


{-| Match a request with the specified header name and value.

    Elmer.Http.expect (Elmer.Http.Route.get "http://fake.com/fake") (
      Elmer.some <| 
        hasHeader ( "x-auth-token", "xxxxx" )
    )

-}
hasHeader : ( String, String ) -> Matcher HttpRequest
hasHeader ( name, value ) =
  \request ->
    if List.isEmpty request.headers then
      Expect.fail (format [ fact "Expected request to have header" (name ++ " = " ++ value), note "but no headers have been set" ])
    else
      let
        filteredHeaders = List.filter (\h -> h.name == name && h.value == value) request.headers
      in
        if List.isEmpty filteredHeaders then
          let
            headers = String.join "\n" (List.map (\h -> h.name ++ " = " ++ h.value) request.headers)
          in
            Expect.fail (format [ fact "Expected request to have header" (name ++ " = " ++ value), fact "but it has" headers])
        else
          Expect.pass
