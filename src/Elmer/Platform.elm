module Elmer.Platform exposing
  ( Spy
  , Calls
  , spy
  , andCallFake
  , expectSpy
  , use
  )

{-| Functions for spying during tests.

# Spy on a Function
@docs Spy, Calls, spy, expectSpy

# Provide a Fake Implementation
@docs andCallFake

# Use a Spy
@docs use

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.Internal as Internal exposing (..)
import Elmer.Platform.Internal as Platform
import Elmer.Printer exposing (..)

{-| Represents a function that has been spied on.
-}
type alias Spy =
  Platform.Spy

{-| Represents the calls made to a spy.
-}
type alias Calls =
  Platform.Calls

{-| Spy on a function

Use a spy when you want to know that a function was called.

To create a spy, pass in a string to identify the spy and a function that returns the
function you want spy on.

To use the spy, pass the `Spy` to `use`. When the spied upon function is called,
Elmer will record that it has been called. By defalut, Elmer will then call
through to the original function.

    let
      mySpy = spy "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] componentState
        |> expectSpy "my-spy" (wasCalled 0)

-}
spy : String -> (() -> a) -> Spy
spy name namingFunc =
  Platform.Spy <|
    \() ->
      Platform.spyOn name namingFunc

{-| Call the provided function when a Spy is called.

Once you've created a `Spy`, you can provide a fake implementation like so:

    mySpy = spy "my-spy" (\_ -> MyComponent.someFunction)
      |> andCallFake testImplementation

where `testImplementation` is some function with the very same signature as
the one being spied upon.

If you are spying on a function that returns a `Cmd`, then your fake
should return one of the fake commands described in `Elmer.Platform.Command`.

For example, you could override `Task.perform` with a fake command that tags
some data like so:

    spy "fake-perform" (\_ -> Task.perform)
      |> andCallFake (\tagger task ->
          Elmer.Platform.Command.fake (tagger "some data")
        )

If you are spying on a function that returns a `Sub`, then your fake should
return a fake subscription; see `Subscription.fake`.

Note: The fake implementation will not be active until you register this spy
via `use`.

-}
andCallFake : (a -> b) -> Spy -> Spy
andCallFake fakeFunction (Platform.Spy spyFunc) =
  Platform.Spy <|
    \() ->
      spyFunc ()
        |> Maybe.andThen (\spyId -> Native.Platform.registerFake spyId fakeFunction)


{-| Make an expectation about a spy.

See `Elmer.Platform.Matchers` for matchers to use with this function.

    let
      mySpy = spy "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] componentState
        |> expectSpy "my-spy" (wasCalled 0)

-}
expectSpy : String -> Matcher Calls -> Elmer.ComponentState model msg -> Expect.Expectation
expectSpy name matcher =
  Internal.mapToExpectation (\component ->
    case Platform.callsForSpy name of
      Just spyCalls ->
        matcher spyCalls
      Nothing ->
        Expect.fail <|
          format
            [ message "Attempted to make expectations about a spy" name
            , description "but it has not been registered as a spy"
            ]
  )


{-| Install stubs for use during the test.

Suppose your component contains a button that,
when clicked, issues a command to get the current date and updates the view. To
get the current date, in your code you'll need to create a `Task` with `Date.now` and then
generate a command with `Task.perform`. To describe this behavior in your test,
you could do something like the following:

    let
      taskOverride = spy "fake-perform" (\_ -> Task.perform)
        |> andCallFake (\tagger task ->
            Elmer.Platform.Command.fake (tagger (toDate "11/12/2016 5:30 pm"))
          )
    in
      componentState
        |> use [ taskOverride ]
        |> Elmer.Html.find "#get-date"
        |> Elmer.Html.Event.click
        |> Elmer.Html.find "#current-date"
        |> Elmer.Html.expectElement (
          Elmer.Html.Matchers.hasText "11/12/2016 5:30 pm"
        )

-}
use : List Spy -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
use stubs =
  Internal.map (\component ->
    case Platform.installSpies stubs of
      Just _ ->
        Ready component
      Nothing ->
        Failed "Failed to install stubs!"
          |> Platform.clearSpies
  )
