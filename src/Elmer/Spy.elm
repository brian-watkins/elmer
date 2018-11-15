module Elmer.Spy exposing
  ( Spy
  , SpyReference
  , Fake
  , Calls
  , on
  , andCallFake
  , andCallThrough
  , replaceValue
  , onFake
  , this
  , callable
  , expect
  , use
  )

{-| Functions for spying during tests.

@docs Spy, Calls

# Spy on a Real Function
@docs SpyReference, on, andCallThrough, andCallFake, replaceValue

# Spy on a Provided Function
@docs Fake, onFake, callable, this

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

{-| Represents a real function to spy on.
-}
type SpyReference a b =
  SpyReference String Spy

{-| Represents a fake function to spy on.
-}
type Fake a b =
  Fake String Spy

{-| Represents the calls made to a spy.
-}
type alias Calls =
  Spy_.Calls


{-| Spy on an existing function.

Use `Spy.on` to identify the function you want spy on. 
Then, call `andCallThrough` or `andCallFake` to specify what should be done
when the spied upon function is called.

    let
      mySpy =
        on "my-spy" (\_ -> MyModule.someFunction)
          |> andCallThrough
    in
      testState
        |> use [ mySpy ]
        |> expect "my-spy" (wasCalled 0)

-}
on : String -> (() -> (a -> b)) -> SpyReference a b
on name identifier =
  SpyReference name <| Spy_.Uninstalled <|
    \() ->
      Spy_.create name identifier


{-| Convert a `Fake` into a `Spy`.

You'll need to call this function with any `Fake` values when you register them with `Elmer.Spy.use`:

    let
      fake =
        Elmer.Spy.onFake "my-spy" (tagger ->
            Command.fake <| tagger "Success!"
          )
      updateForTest =
        MyModule.updateUsing <|
          Elmer.Spy.callable fake
    in
      Elmer.given testModel MyModule.view updateForTest
        |> Elmer.Spy.use [ Elmer.Spy.this fake ]
        |> Elmer.Html.target
            << by [ tag "input" ]
        |> Elmer.Html.Event.input "some text"
        |> Elmer.Html.target
            << by [ tag "button" ]
        |> Elmer.Html.Event.click
        |> Elmer.Spy.expect "my-spy" (
          Elmer.Spy.Matchers.wasCalledWith
            [ Elmer.Spy.Matchers.stringArg "some text"
            ]
        )
-}
this : Fake a b -> Spy
this (Fake _ spy) =
  spy


{-| Provide a function to be spied upon during a test.

For example, let's say you want to inject some dependencies into your update
function to decouple application logic from view logic. In your tests, maybe you
need a fake version of the injected function. You would call `onFake` with the 'fake'
function. Then, use `Elmer.Spy.callable` when you want
to provide a version of the 'fake' function that will record its calls.

    let
      fake =
        Elmer.Spy.onFake "my-spy" (tagger ->
          Command.fake <| tagger "Success!"
        )
      updateForTest =
        MyModule.updateUsing <|
          Elmer.Spy.callable fake
    in
      Elmer.given testModel MyModule.view updateForTest
        |> Elmer.Spy.use [ Elmer.Spy.this fake ]
        |> Elmer.Html.target
            << by [ tag "input" ]
        |> Elmer.Html.Event.input "some text"
        |> Elmer.Html.target
            << by [ tag "button" ]
        |> Elmer.Html.Event.click
        |> Elmer.Spy.expect "my-spy" (
          Elmer.Spy.Matchers.wasCalledWith
            [ Elmer.Spy.Matchers.stringArg "some text"
            ]
        )
-}
onFake : String -> (a -> b) -> Fake a b
onFake name fakeFunction =
  Fake name <| Spy_.Uninstalled <|
    \() ->
      Spy_.createWith name fakeFunction


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


{-| Returns the function represented by the `Fake`.

Once you register the `Fake` as a `Spy` via `Elmer.Spy.use`
then calls to the fake will be recorded as expected.

For example:

    let
      fake =
        Elmer.Spy.onFake "my-spy" (tagger ->
          Command.fake <| tagger "Success!"
        )
      updateForTest =
        MyModule.updateUsing <|
          Elmer.Spy.callable fake
    in
      Elmer.given testModel MyModule.view updateForTest
        |> Elmer.Spy.use [ Elmer.Spy.this fake ]
        |> Elmer.Html.target
            << by [ tag "input" ]
        |> Elmer.Html.Event.input "some text"
        |> Elmer.Html.target
            << by [ tag "button" ]
        |> Elmer.Html.Event.click
        |> Elmer.Spy.expect "my-spy" (
          Elmer.Spy.Matchers.wasCalledWith
            [ Elmer.Spy.Matchers.stringArg "some text"
            ]
        )
-}
callable : Fake a b -> (a -> b)
callable (Fake name _) =
  Spy_.callable name


{-| Call the provided function when a `Spyable` function is called.

Once you've created a `Spyable`, you can provide a fake implementation like so:

    mySpy =
      Elmer.Spy.on "my-spy" (\_ ->
        MyModule.someFunction
      )
      |> andCallFake testImplementation

where `testImplementation` is some function with the very same signature as
the one being spied upon.

If you are spying on a function that returns a `Cmd`, then your fake
should return `Cmd.none` or one of the fake commands described in `Elmer.Command`.

For example, you could override `Random.generate` so that it returns a set value during a test
like so:

    Elmer.Spy.on "fake-random" (\_ ->
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
andCallFake fakeFunction (SpyReference _ spy) =
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

{-| Call through to the existing implementation of a `Spyable` function.

Once you've created a `Spyable`, you can just call `andCallThrough` to have Elmer call the original
implementation whenever the function is called. 

    mySpy =
      Elmer.Spy.on "my-spy" (\_ ->
        MyModule.someFunction
      )
      |> andCallThrough

Use `andCallThrough` when you want to examine the arguments passed to a function and don't need to 
change what that function does for your tests.

Note: The Spy will not be active until you register it via `Elmer.Spy.use`.
-}
andCallThrough : SpyReference a b -> Spy
andCallThrough (SpyReference _ spy) =
  spy


{-| Make an expectation about a spy.

See `Elmer.Spy.Matchers` for matchers to use with this function.

    let
      mySpy =
        Elmer.Spy.on "my-spy" (\_ -> 
          MyModule.someFunction
        )
        |> andCallThrough
    in
      testState
        |> use [ mySpy ]
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
when clicked, issues a command to get a random number and updates the view. To
get the random number, in your code you'll need to use `Random.generate` with the
appropriate `Generator`. To describe this behavior in your test,
you could do something like the following:

    let
      randomSpy =
        Elmer.Spy.on "fake-random" (\_ -> Random.generate)
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
