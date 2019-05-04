module Elmer.Effects exposing
  ( push
  , expect
  , use
  )

{-| Functions for working with effects during a test.

Functions that return `Cmd` values to the Elm runtime may have side effects that you
want to track during a test. For instance, the `Elmer.Http` module will keep track of 
requests that would be sent when the Elm runtime processes a given command so you can
make expectations about them later.

Note that these functions are mainly useful when writing extensions to Elmer
or custom matchers.

@docs push, expect, use

-}

import Expect
import Elmer.TestState as TestState exposing (TestState)
import Elmer.Context as Context
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Errors as Errors


{-| Create a command that records an effect. 

Provide a custom type as an 'effectId' and then a function that produces the
new value for the stored effects based on what has (or has not) been stored already.

This function produces a command that should be sent to the Elmer runtime, either in
the normal course of code that's been exercised or via `Elmer.Command.send`. 

For example, suppose you have an effect id like:

    type Effects =
      Effects

You could record effects like so:

    testState
      |> Command.send (\_ -> push Effects (\_ -> "Hello!"))

-}
push : effectId -> (Maybe a -> a) -> Cmd msg
push =
  RuntimeCommand.mapState


{-| Use a recorded effect during a test.

This function allows you to access the currently recorded effects during a test to produce
further steps in that test. For example, if your test stored a list of effects with the id `Effects`
then you could clear that list like so:

    testState
      |> use Effects (\_ state ->
        state
          |> Command.send (\_ -> Effects.push Effects (\_ -> []))
      )

-}
use : effectId -> (Maybe a -> TestState model msg -> TestState model msg) -> TestState model msg -> TestState model msg
use effectId mapper testState =
  testState
    |> TestState.mapWithoutSpies (
        \context ->
          mapper (Context.state effectId context) testState
    )


{-| Make an expectation about stored effects.

    testState
      |> Command.send (\_ -> push Effects (\_ -> "Hello!"))
      |> expect Effects (\maybeEffect ->
        Maybe.withDefault "" maybeEffect
          |> Expect.equal "Hello!"
      )

-}
expect : effectId -> (Maybe a -> Expect.Expectation) -> TestState model msg -> Expect.Expectation
expect effectId mapper =
  TestState.mapToExpectation <|
    \context ->
      Context.state effectId context
        |> mapper
