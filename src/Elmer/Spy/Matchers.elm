module Elmer.Spy.Matchers exposing
  ( wasCalled
  )

{-| Matchers for making expectations about Spies

@docs wasCalled

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.Spy exposing (Calls)
import Elmer.Printer exposing (..)


{-| Expect that a spy was called some number of times.
-}
wasCalled : Int -> Matcher Calls
wasCalled times spy =
  if spy.calls == times then
    Expect.pass
  else
    Expect.fail <|
      format
        [ message ("Expected spy " ++ spy.name ++ " to have been called") <| timesString times
        , message "but it was called" <| timesString spy.calls
        ]

timesString : Int -> String
timesString times =
  if times == 1 then
    (toString times) ++ " time"
  else
    (toString times) ++ " times"
