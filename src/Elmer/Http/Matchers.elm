module Elmer.Http.Matchers exposing
  ( hasAnyBody
  , hasBody
  , exists
  )

import Expect
import Elmer.Types exposing (..)
import Elmer.Http exposing (..)

exists : HttpRequestData -> Expect.Expectation
exists request =
  Expect.pass

hasAnyBody : HttpRequestData -> Expect.Expectation
hasAnyBody request =
  case request.body of
    Just _ ->
      Expect.pass
    Nothing ->
      Expect.fail "Expected request to have a body but it does not"

hasBody : String -> HttpRequestData -> Expect.Expectation
hasBody expectedBody request =
  case request.body of
    Just body ->
      if body == expectedBody then
        Expect.pass
      else
        Expect.fail ("Expected request to have body\n\n\t" ++ expectedBody ++ "\n\nbut it has\n\n\t" ++ body)
    Nothing ->
      Expect.fail ("Expected request to have body\n\n\t" ++ expectedBody ++ "\n\nbut it has no body")
