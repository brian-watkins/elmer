module Elmer.Http.Matchers exposing
  ( hasAnyBody
  , hasBody
  , wasSent
  , hasQueryParam
  , hasHeader
  )

{-| Make expectations about Http requests sent by the component under test.

These matchers should be used with `Elmer.Http.expectGET` etc.

Note: Your test must use `Elmer.Http.serve` or `Elmer.Http.spy` at the
appropriate time to allow Elmer to record the requests sent by the component
under test.

# Matchers
@docs wasSent, hasAnyBody, hasBody, hasQueryParam, hasHeader
-}

import Expect
import Http
import Elmer exposing (Matcher)
import Elmer.Http.Request exposing (HttpRequest)
import Elmer.Printer exposing (..)


{-| Match any request with the proper method and route as specified in
`Elmer.Http.expectGET` etc.

    expectGET "http://fake.com/fake" wasSent

-}
wasSent : Matcher HttpRequest
wasSent request =
  Expect.pass


{-| Match any request that has a body.
-}
hasAnyBody : Matcher HttpRequest
hasAnyBody request =
  case request.body of
    Just _ ->
      Expect.pass
    Nothing ->
      Expect.fail (formatMessage (description "Expected request to have a body but it does not"))


{-| Match a request with the specified body.

    expectPOST "http://fake.com/fake" (
      hasBody "{\"name\":\"Fun Person\"}"
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

    expectGET "http://fake.com/fake" (
      hasQueryParam ( "name", "Fun Person" )
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

    expectGET "http://fake.com/fake" (
      hasHeader ( "x-auth-token", "xxxxx" )
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
