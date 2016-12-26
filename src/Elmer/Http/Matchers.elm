module Elmer.Http.Matchers exposing
  ( hasAnyBody
  , hasBody
  , hasBeenRequested
  , hasQueryParam
  )

import Expect
import Http
import Elmer.Types exposing (..)
import Elmer.Http exposing (..)
import Elmer.Printer exposing (..)

hasBeenRequested : HttpRequestData -> Expect.Expectation
hasBeenRequested request =
  Expect.pass

hasAnyBody : HttpRequestData -> Expect.Expectation
hasAnyBody request =
  case request.body of
    Just _ ->
      Expect.pass
    Nothing ->
      Expect.fail (formatMessage (description "Expected request to have a body but it does not"))

hasBody : String -> HttpRequestData -> Expect.Expectation
hasBody expectedBody request =
  case request.body of
    Just body ->
      if body == expectedBody then
        Expect.pass
      else
        Expect.fail (format [ message "Expected request to have body" expectedBody, message "but it has" body ])
    Nothing ->
      Expect.fail (format [ message "Expected request to have body" expectedBody, description "but it has no body" ])

hasQueryParam : ( String, String ) -> HttpRequestData -> Expect.Expectation
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

queryString : HttpRequestData -> String
queryString request =
  String.split "?" request.url
    |> List.drop 1
    |> List.head
    |> Maybe.withDefault ""
