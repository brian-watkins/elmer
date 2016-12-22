module Elmer.Http.Matchers exposing
  ( hasAnyBody
  , hasBody
  , hasBeenRequested
  )

import Expect
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
