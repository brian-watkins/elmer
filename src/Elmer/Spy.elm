module Elmer.Spy exposing
  ( Spy
  , Calls
  , create
  , createWith
  , replaceValue
  , callable
  , andCallFake
  , expect
  , use
  )

{-| Functions for spying during tests.

@docs Spy, Calls

# Spy on a Real Function
@docs create, andCallFake, replaceValue

# Spy on a Provided Function
@docs createWith, callable

# Use a Spy
@docs use

# Make Expectations about a Spy
@docs expect

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.TestState as TestState
import Elmer.Spy.Internal as Spy_ exposing (Spy(..))
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

To create a spy for a function, pass in a string to identify the spy and a function that returns the
function you want spy on.

Use a spy when you want to know that a function was called.

To use the spy, pass the `Spy` to `use`. When the spied upon function is called,
Elmer will record that it has been called. By defalut, Elmer will then call
through to the original function.

    let
      mySpy =
        create "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] testState
        |> expect "my-spy" (wasCalled 0)

-}
create : String -> (() -> a) -> Spy
create name namingFunc =
  Spy_.Uninstalled <|
    \() ->
      Spy_.create name namingFunc


{-| Create a spy for a function you provide.

Let's say you're testing a function that has a function for one of its arguments.
In your test, you may want to provide some function that simulates certain
conditions. If you want to assert that the provided function is itself called
with certain arguments, use `createWith` to construct a spy for that function.

When you pass the spied upon function to the function under test, use `Spy.call`
which will return a version of the function that records its calls.

For example, let's say you want to inject some dependencies into your update
function to decouple application logic from view logic. You would create a spy
with a function you provide for your test. Then, use `Spy.callable` when you want
to provide a version of the function that will record its calls.

    let
      spy =
        createWith "my-spy" (tagger ->
          Command.fake <| tagger "Success!"
        )
      updateForTest =
        MyModule.updateUsing <|
          Spy.callable "my-spy"
    in
      Elmer.given testModel MyModule.view updateForTest
        |> Elmer.Spy.use [ spy ]
        |> Elmer.Html.target "input"
        |> Elmer.Html.Event.input "some text"
        |> Elmer.Html.target "button"
        |> Elmer.Html.Event.click
        |> Elmer.Spy.expect "my-spy" (
          Elmer.Spy.Matchers.wasCalledWith
            [ Elmer.Spy.Matchers.stringArg "some text"
            ]
        )

Note: Using `andCallFake` with a spy produced via `createWith` will replace the
provided function.
-}
createWith : String -> (a -> b) -> Spy
createWith name fakeFunction =
  Spy_.Uninstalled <|
    \() ->
      Spy_.createWith name fakeFunction


{-| Stub a function that simply returns a value.

Suppose you have a function like `Time.now` that takes no arguments and simply
returns a value. You can specify the value returned by such a function during your
test like so:

    timeNowSpy : Spy
    timeNowSpy =
      Success 1000
        |> Elmer.Task.fake
        |> Spy.replaceValue (\_ -> Time.now)

Note: It's not possible to make expectations about spies constructed
with `replaceValue`.

Note: An error will result if you attempt to use `replaceValue`
with a function that has arguments.

-}
replaceValue : (() -> a) -> b -> Spy
replaceValue namingFunc value =
  Spy_.Uninstalled <|
    \() ->
      Spy_.replaceValue namingFunc value


{-| Returns a function that records calls to itself and calls through to the function
associated with the spy with the given name.

Note: Use `callable` only in conjunction with spies produced using `createWith`; otherwise
you'll receive an error.
-}
callable : String -> (a -> b)
callable =
  Spy_.callable


{-| Call the provided function when a Spy is called.

Once you've created a `Spy`, you can provide a fake implementation like so:

    mySpy =
      create "my-spy" (\_ -> MyComponent.someFunction)
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
andCallFake fakeFunction spy =
  case spy of
    Uninstalled installer ->
      Spy_.Uninstalled <|
        \() ->
          let
            installed = installer ()
          in
            case installed of
              Active spyValue ->
                Spy_.registerFake fakeFunction spyValue
              _ ->
                installed
    _ ->
      spy


{-| Make an expectation about a spy.

See `Elmer.Spy.Matchers` for matchers to use with this function.

    let
      mySpy =
        create "my-spy" (\_ -> MyComponent.someFunction)
    in
      use [ mySpy ] testState
        |> expect "my-spy" (wasCalled 0)

-}
expect : String -> Matcher Calls -> Elmer.TestState model msg -> Expect.Expectation
expect name matcher =
  TestState.mapToExpectation (\context ->
    case Spy_.calls name <| Spy_.spiesFrom context of
      Just calls ->
        matcher calls
      Nothing ->
        Expect.fail <|
          format
            [ message "Attempted to make expectations about a spy" name
            , description "but it has not been registered as a spy"
            ]
  )

{-| Install spies for use during the test.

Suppose your component contains a button that,
when clicked, issues a command to get the current date and updates the view. To
get the current date, in your code you'll need to create a `Task` with `Date.now` and then
generate a command with `Task.perform`. To describe this behavior in your test,
you could do something like the following:

    let
      taskOverride =
        create "fake-perform" (\_ -> Task.perform)
          |> andCallFake (\tagger task ->
            Elmer.Platform.Command.fake (
              tagger <| toDate "11/12/2016 5:30 pm"
            )
          )
    in
      testState
        |> use [ taskOverride ]
        |> Elmer.Html.target "#get-date"
        |> Elmer.Html.Event.click
        |> Elmer.Html.target "#current-date"
        |> Elmer.Html.expect (
          Elmer.Html.Matchers.element <|
            Elmer.Html.Matchers.hasText "11/12/2016 5:30 pm"
        )

Note: If you need to replace a spy during the course of a test, you may
call `use` again with the new spy. Each time you call `use` *all* spies
will be removed. So be sure that each time you call `use` you register all
the spies you need.
-}
use : List Spy -> Elmer.TestState model msg -> Elmer.TestState model msg
use spies =
  TestState.mapWithoutSpies <|
    \context ->
      let
        activated = Spy_.activate spies
        errors = takeErrors activated
      in
        if List.isEmpty errors then
          Spy_.deactivate activated
            |> flip Spy_.withSpies context
            |> TestState.with
        else
          TestState.failure <|
            format
              [ message "Failed to activate spies" <| failedSpies errors ]


failedSpies : List Spy -> String
failedSpies spies =
  List.filterMap (\spy ->
    case spy of
      Error spyValue ->
        Just spyValue.name
      _ ->
        Nothing
  ) spies
    |> String.join "\n"

takeErrors : List Spy -> List Spy
takeErrors =
  List.filter (\spy ->
    case spy of
      Error spyValue ->
        True
      _ ->
        False
  )
