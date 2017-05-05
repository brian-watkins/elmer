module Elmer.Http.Matchers exposing
  ( wasRequested
  , hasBody
  , hasQueryParam
  , hasHeader
  )

{-| Make expectations about Http requests sent by the component under test.

These matchers should be used with `Elmer.Http.expectGET` etc.

Note: Your test must use `Elmer.Http.serve` or `Elmer.Http.spy` at the
appropriate time to allow Elmer to record the requests sent by the component
under test.

# Matchers
@docs wasRequested, hasBody, hasQueryParam, hasHeader
-}

import Expect
import Http
import Elmer exposing (Matcher)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Printer exposing (..)


{-| Expect that some number of requests have been recorded.

    Elmer.Http.expectThat (Elmer.Http.Route.get "http://fun.com/fun.html") (
      wasRequested 3
    )
-}
wasRequested : Int -> Matcher (List HttpRequest)
wasRequested times requests =
  if List.length requests == times then
    Expect.pass
  else
    Expect.fail <|
      "Expected "
        ++ (toString times)
        ++ " "
        ++ (pluralize "request" times)
        ++ ", but recorded "
        ++ (toString <| List.length requests)
        ++ " "
        ++ (pluralize "request" (List.length requests))

pluralize : String -> Int -> String
pluralize word num =
  if num > 1 || num == 0 then
    word ++ "s"
  else
    word

{-| Match a request with the specified body.

    Elmer.Http.expectThat (Elmer.Http.Route.post "http://fake.com/fake") (
      Elmer.some <| hasBody "{\"name\":\"Fun Person\"}"
    )

-}
hasBody : String -> Matcher HttpRequest
hasBody expectedBody request =
  case request.body of
    Just body ->
      if body == expectedBody then
        Expect.pass
      else
        Expect.fail (format [ message "Expected request to have body" expectedBody, message "but it has" body ])
    Nothing ->
      Expect.fail (format [ message "Expected request to have body" expectedBody, description "but it has no body" ])


{-| Match a request that has a query string containing the specified name and value.

Note: You don't need to worry about url encoding the name or value.

    Elmer.Http.expectThat (Elmer.Http.Route.get "http://fake.com/fake") (
      Elmer.some <| hasQueryParam ( "name", "Fun Person" )
    )

-}
hasQueryParam : ( String, String ) -> Matcher HttpRequest
hasQueryParam ( key, value ) request =
  let
    query = queryString request
    params = String.split "&" query
    expectedParam = (Http.encodeUri key) ++ "=" ++ (Http.encodeUri value)
  in
    if String.isEmpty query then
      Expect.fail (format [ message "Expected request to have query param" ( key ++ " = " ++ value), description "but it has no query string" ])
    else if List.member expectedParam params then
      Expect.pass
    else
      Expect.fail (format [ message "Expected request to have query param" ( key ++ " = " ++ value), message "but it has" query ])

queryString : HttpRequest -> String
queryString request =
  String.split "?" request.url
    |> List.drop 1
    |> List.head
    |> Maybe.withDefault ""


{-| Match a request with the specified header name and value.

    Elmer.Http.expectThat (Elmer.Http.Route.get "http://fake.com/fake") (
      Elmer.some <| hasHeader ( "x-auth-token", "xxxxx" )
    )

-}
hasHeader : ( String, String ) -> Matcher HttpRequest
hasHeader ( name, value ) request =
  if List.isEmpty request.headers then
    Expect.fail (format [ message "Expected request to have header" (name ++ " = " ++ value), description "but no headers have been set" ])
  else
    let
      filteredHeaders = List.filter (\h -> h.name == name && h.value == value) request.headers
    in
      if List.isEmpty filteredHeaders then
        let
          headers = String.join "\n" (List.map (\h -> h.name ++ " = " ++ h.value) request.headers)
        in
          Expect.fail (format [ message "Expected request to have header" (name ++ " = " ++ value), message "but it has" headers])
      else
        Expect.pass
