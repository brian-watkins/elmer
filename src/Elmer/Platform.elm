module Elmer.Platform exposing
  ( Spy
  , Stub
  , spy
  , expectSpy
  , stub
  , batchStub
  , use
  )

{-| Functions for spying on and stubbing functions during tests.

# Spy on a Function
@docs Spy, spy, expectSpy

# Stub a Function
@docs Stub, stub, batchStub, use

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

{-| Spy on a function

Use a spy when you want to know that a function was called, but you don't care to
describe the consequences of that function.

Pass in a string to identify the spy and a function that returns the
function you want spy on. When the spied upon function is called, Elmer will record that it
has been called and then call through to the original function.

    spy "my-spy" (\_ -> MyComponent.someFunction) componentState
      |> expectSpy "my-spy" (wasCalled 0)

-}
spy : String -> (() -> a) -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
spy name func =
  use [ spyStub name func ]

spyStub : String -> (() -> a) -> Platform.Stub
spyStub name namingFunc =
  Platform.Stub <|
    \() ->
      Platform.spyOn name namingFunc


{-| Make an expectation about a spy.

    spy "my-spy" (\_ -> MyComponent.someFunction) componentState
      |> expectSpy "my-spy" (wasCalled 0)

-}
expectSpy : String -> Matcher Spy -> Elmer.ComponentState model msg -> Expect.Expectation
expectSpy name matcher =
  Internal.mapToExpectation (\component ->
    case Platform.spyData name of
      Just spyData ->
        matcher spyData
      Nothing ->
        Expect.fail <|
          format
            [ message "Attempted to make expectations about a spy" name
            , description "but it has not been registered as a spy"
            ]
  )

{-| Represents a stubbed function.
-}
type alias Stub
  = Platform.Stub

{-| Combine a List of Stubs into a single Stub.
-}
batchStub : List Stub -> Stub
batchStub overrides =
  Platform.Stub <|
    \() ->
      Platform.installStubs overrides


{-| Stub a function.

The first argument is a function that simply returns the function you want to
stub. The second argument is a function with the same signature as the function
to stub. It should return whatever you need it to return for your test.

If you are stubbing a function that returns a `Cmd`, then your stub should return
one of the fake commands described in `Elmer.Platform.Command`.

If you are stubbing a function that returns a `Sub`, then your stub should
return a fake subscription; see `Subscription.fake`.

Note: This function merely creates a description of the stub; the function
is not actually stubbed until you call `Elmer.Platform.use`.

You could override `Task.perform` with a fake command that tags some data like so:

    stub (\_ -> Task.perform) (\tagger task ->
      Elmer.Platform.Command.fake (tagger "some data")
    )

-}
stub : (() -> a) -> (b -> c) -> Stub
stub namingFunc stubbingFunc =
  Platform.Stub <|
    \() ->
      Platform.stub namingFunc stubbingFunc

{-| Install stubs for use during the test.

Suppose your component contains a button that,
when clicked, issues a command to get the current date and updates the view. To
get the current date, in your code you'll need to create a `Task` with `Date.now` and then
generate a command with `Task.perform`. To describe this behavior in your test,
you could do something like the following:

    let
      taskOverride = stub (\_ -> Task.perform) (\tagger task ->
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
use : List Stub -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
use stubs =
  Internal.map (\component ->
    if Platform.installStubs stubs then
      Ready component
    else
      Failed "Failed to install stubs!"
        |> Platform.clearStubs
  )
