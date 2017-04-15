module Elmer.Spy exposing
  ( Spy
  , Calls
  , create
  , andCallFake
  , expect
  , use
  )

{-| Functions for spying during tests.

# Spy on a Function
@docs Spy, Calls, create, expect

# Provide a Fake Implementation
@docs andCallFake

# Use a Spy
@docs use

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.Internal as Internal exposing (..)
import Elmer.Spy.Internal as Spy_
import Elmer.Printer exposing (..)

{-| Represents a function that has been spied on.
-}
type alias Spy =
  Spy_.Spy

{-| Represents the calls made to a spy.
-}
type alias Calls =
  Spy_.Calls

{-| Create a spy for a function.

To create a spy, pass in a string to identify the spy and a function that returns the
function you want spy on.

Use a spy when you want to know that a function was called.

To use the spy, pass the `Spy` to `use`. When the spied upon function is called,
Elmer will record that it has been called. By defalut, Elmer will then call
through to the original function.

    let
      mySpy = create "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] componentState
        |> expect "my-spy" (wasCalled 0)

-}
create : String -> (() -> a) -> Spy
create name namingFunc =
  Spy_.Spy <|
    \() ->
      Spy_.spyOn name namingFunc

{-| Call the provided function when a Spy is called.

Once you've created a `Spy`, you can provide a fake implementation like so:

    mySpy = create "my-spy" (\_ -> MyComponent.someFunction)
      |> andCallFake testImplementation

where `testImplementation` is some function with the very same signature as
the one being spied upon.

If you are spying on a function that returns a `Cmd`, then your fake
should return one of the fake commands described in `Elmer.Platform.Command`.

For example, you could override `Task.perform` with a fake command that tags
some data like so:

    create "fake-perform" (\_ -> Task.perform)
      |> andCallFake (\tagger task ->
          Elmer.Platform.Command.fake (tagger "some data")
        )

If you are spying on a function that returns a `Sub`, then your fake should
return a fake subscription; see `Subscription.fake`.

Note: The fake implementation will not be active until you register this spy
via `use`.

-}
andCallFake : (a -> b) -> Spy -> Spy
andCallFake fakeFunction (Spy_.Spy spyFunc) =
  Spy_.Spy <|
    \() ->
      spyFunc ()
        |> Maybe.andThen (\spyId -> Native.Spy.registerFake spyId fakeFunction)


{-| Make an expectation about a spy.

See `Elmer.Spy.Matchers` for matchers to use with this function.

    let
      mySpy = create "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] componentState
        |> expect "my-spy" (wasCalled 0)

-}
expect : String -> Matcher Calls -> Elmer.ComponentState model msg -> Expect.Expectation
expect name matcher =
  Internal.mapToExpectation (\component ->
    case Spy_.callsForSpy name of
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
      taskOverride = create "fake-perform" (\_ -> Task.perform)
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
    case Spy_.installSpies stubs of
      Just _ ->
        Ready component
      Nothing ->
        Failed "Failed to install stubs!"
          |> Spy_.clearSpies
  )
