module Elmer.Task exposing
  ( failTest
  )

{-| Functions to produce Tasks to use during a test.

To work with Tasks during a test, you should 

1. Stub any functions that generate a Task with `Task.succeed` or `Task.fail`
2. That's it.

For example, you could write a test that exercises code using `Browser.Dom.focus` like so:

    focusStub : Spy
    focusStub =
      Elmer.Spy.observe (\_ -> Browser.Dom.focus)
        |> Elmer.Spy.andCallFake (\_ -> 
          Task.succeed ()
        )

    Elmer.given testModel testView testUpdate
      |> Spy.use [ focusStub ]
      |> etc ...

In this case, the focusStub would allow you to simulate the success of the task produced by
`Browser.Dom.focus`.

Elmer knows how to process Tasks in general (eg, functions like `Task.map`, `Task.andThen`, etc),
so you only need to stub functions that produce Tasks from modules other than the elm/core `Task` module.

# Special Tasks
@docs failTest

-}

import Task exposing (Task)
import Elmer.Runtime.Task as RuntimeTask
import Elmer.Command as Command


{-| Generate a Task that will cause the test to fail with the given message.
-}
failTest : String -> Task x a
failTest failureMessage =
  Command.fail failureMessage
    |> RuntimeTask.abortWith