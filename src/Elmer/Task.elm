module Elmer.Task exposing
  ( TaskResult(..)
  , fake
  )

{-| Functions for dealing with Tasks during your tests.

# Create a Fake Task
@docs TaskResult, fake

-}

import Task exposing (Task)
import Json.Encode as Encode exposing (Value)
import Elmer.Value as Value


{-| Characterize the value to be resolved by a fake task.
-}
type TaskResult x a
  = Success a
  | Failure x


{-| Create a fake task.
-}
fake : TaskResult x a -> Task x a
fake taskResult =
  case taskResult of
    Success value ->
      taskValue "_Elmer_Task_succeed" value
        |> Value.cast
    Failure value ->
      taskValue "_Elmer_Task_fail" value
        |> Value.cast


taskValue : String -> a -> Value
taskValue taskType value =
  Encode.object
    [ ( "ctor", Encode.string taskType )
    , ( "value", Value.cast value)
    ]
