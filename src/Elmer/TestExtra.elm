module Elmer.TestExtra exposing
  ( not
  , andThen
  )

import Expect

not : Expect.Expectation -> Expect.Expectation
not expectation =
  if expectation == Expect.pass then
    Expect.fail "Expected to fail, but it passed"
  else
    Expect.pass

andThen : Expect.Expectation -> Expect.Expectation -> Expect.Expectation
andThen firstExpect secondExpect =
  if firstExpect == Expect.pass then
    secondExpect
  else
    firstExpect
