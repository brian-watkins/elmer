module Elmer.Spy exposing
  ( Spy
  , SpyReference
  , Calls
  , observe
  , andCallFake
  , andCallThrough
  , replaceValue
  , inject
  , expect
  , use
  )

{-| Functions for spying during tests.

@docs Spy, Calls

# Spy on a Function
@docs SpyReference, observe, andCallThrough, andCallFake, replaceValue

# Use a Spy
@docs inject, use

# Make Expectations about a Spy
@docs expect

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.TestState as TestState
import Elmer.Spy.Internal as Spy_ exposing (Spy(..))
import Elmer.Spy.Function as Function
import Elmer.Errors as Errors exposing (failWith)

{-| Represents a function that has been spied on.
-}
type alias Spy =
  Spy_.Spy

{-| Represents a real function to spy on.
-}
type SpyReference a b =
  SpyReference Spy


{-| Represents the calls made to a spy.
-}
type alias Calls =
  Spy_.Calls


{-| Spy on a function.

Use `Spy.observe` to identify the function you want spy on. 
Then, call `andCallThrough` or `andCallFake` to specify what should be done
when the spied upon function is called.

    let
      mySpy =
        observe (\_ -> MyModule.someFunction)
          |> andCallThrough
    in
      testState
        |> use [ mySpy ]
        |> expect (\_ -> MyModule.someFunction) (
          wasCalled 0
        )

-}
observe : (() -> (a -> b)) -> SpyReference a b
observe identifier =
  SpyReference <| Spy_.Uninstalled <|
    \() ->
      Spy_.create identifier


{-| Stub a function that simply returns a value.

Suppose you have a function like `Time.now` that takes no arguments and simply
returns a value. You can specify the value returned by such a function during your
test like so:

    timeNowSpy : Spy
    timeNowSpy =
      Task.succeed (Time.millisToPosix 1000)
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


{-| Provide a function you want to spy on to the subject under test.

Sometimes, you might want to provide a fake version of some dependency
to the subject under test. If you also want to spy on that fake dependency
then you will need to do three things. First, create that fake
function in your test module. Second, use `Elmer.Spy.inject` to provide the function
to the subject under test. Third, create and use a `Spy` that observes your fake dependency.

    fakeFunction tagger =
      Command.fake <| tagger "Success!"

    test =
      let
        spy =
          Elmer.Spy.observe (\_ -> fakeFunction)
            |> Elmer.Spy.andCallThrough
        updateForTest =
          MyModule.updateUsing <|
            Spy.inject (\_ -> fakeFunction)
      in
        Elmer.given testModel MyModule.view updateForTest
          |> Elmer.Spy.use [ spy ]
          |> Elmer.Html.target
              << by [ tag "input" ]
          |> Elmer.Html.Event.input "some text"
          |> Elmer.Html.target
              << by [ tag "button" ]
          |> Elmer.Html.Event.click
          |> Elmer.Spy.expect (\_ -> fakeFunction) (
            Elmer.Spy.Matchers.wasCalledWith
              [ Elmer.Spy.Matchers.stringArg "some text"
              ]
          )

Note: `Elmer.Spy.inject` is necessary because, without wrapping the function to be observed in a
thunk, there's no opportunity to spy on the function during the test.
-}
inject : (() -> a -> b) -> a -> b
inject thunk arg =
  thunk () <| arg


{-| Call the provided function when a spied-upon function is called.

Once you're observing a function, you can provide a fake implementation like so:

    mySpy =
      Elmer.Spy.observe (\_ ->
        MyModule.someFunction
      )
      |> andCallFake testImplementation

where `testImplementation` is some function with the very same signature as
the one being spied upon.

If you are spying on a function that returns a `Cmd`, then your fake
should return `Cmd.none` or one of the fake commands described in `Elmer.Command`.

For example, you could override `Random.generate` so that it returns a set value during a test
like so:

    Elmer.Spy.observe (\_ ->
      Random.generate
    )
    |> Elmer.Spy.andCallFake (\tagger generator ->
      Random.initialeSeed 10001
        |> Random.step generator
        |> tagger
        |> Elmer.Command.fake
    )

If you are spying on a function that returns a `Sub`, then your fake should
return a fake subscription; see `Elmer.Subscription.fake`.

Note: The fake implementation will not be active until you register this spy
via `Elmer.Spy.use`.
-}
andCallFake : (a -> b) -> SpyReference a b -> Spy
andCallFake fakeFunction (SpyReference spy) =
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

{-| Call through to the existing implementation of a spied-upon function.

Once you're observing a function, you can just call `andCallThrough` to have Elmer call the original
implementation whenever the function is called. 

    mySpy =
      Elmer.Spy.observe (\_ ->
        MyModule.someFunction
      )
      |> andCallThrough

Use `andCallThrough` when you want to examine the arguments passed to a function and don't need to 
change what that function does for your tests.

Note: The Spy will not be active until you register it via `Elmer.Spy.use`.
-}
andCallThrough : SpyReference a b -> Spy
andCallThrough (SpyReference spy) =
  spy


{-| Make an expectation about a spy.

See `Elmer.Spy.Matchers` for matchers to use with this function.

    let
      mySpy =
        Elmer.Spy.observe (\_ -> 
          MyModule.someFunction
        )
        |> andCallThrough
    in
      testState
        |> use [ mySpy ]
        |> expect (\_ -> MyModule.someFunction) (
          wasCalled 0
        )
-}
expect : (() -> a -> b) -> Matcher Calls -> Elmer.TestState model msg -> Expect.Expectation
expect identifier matcher =
  TestState.mapToExpectation <| \context ->
    case Function.functionIdentifier identifier of
      Just name ->
        case Spy_.calls name <| Spy_.spiesFrom context of
          Just calls ->
            matcher calls
          Nothing ->
            failWith <| Errors.unknownSpy name
      Nothing ->
        failWith Errors.badSpyIdentifier


{-| Install spies for use during the test.

Suppose your component contains a button that,
when clicked, issues a command to get a random number and updates the view. To
get the random number, in your code you'll need to use `Random.generate` with the
appropriate `Generator`. To describe this behavior in your test,
you could do something like the following:

    let
      randomSpy =
        Elmer.Spy.observe (\_ -> Random.generate)
        |> andCallFake (\tagger _ ->
          tagger 27
            |> Elmer.Command.fake
        )
    in
      testState
        |> use [ randomSpy ]
        |> Elmer.Html.target 
            << by [ id "get-random" ]
        |> Elmer.Html.Event.click
        |> Elmer.Html.target
            << by [ id "current-random" ]
        |> Elmer.Html.expect (
          Elmer.Html.Matchers.element <|
            Elmer.Html.Matchers.hasText "27"
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
            |> Spy_.withSpiesFor context
            |> TestState.with
        else
          spyErrors errors
            |> Errors.failedToActivateSpies 
            |> Errors.print
            |> TestState.failure


spyErrors : List Spy -> String
spyErrors spies =
  List.filterMap (\spy ->
    case spy of
      Error error ->
        Just <| error.reason
      _ ->
        Nothing
  ) spies
    |> String.join "\n"


takeErrors : List Spy -> List Spy
takeErrors =
  List.filter (\spy ->
    case spy of
      Error spyError ->
        True
      _ ->
        False
  )
