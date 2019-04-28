module Elmer.Spy.Matchers exposing
  ( Arg
  , Call
  , stringArg
  , intArg
  , floatArg
  , boolArg
  , typedArg
  , functionArg
  , anyArg
  , argThat
  , wasCalled
  , wasCalledWith
  , calls
  , hasArgs
  )

{-| Matchers for making expectations about Spies.

# General Expectations
@docs wasCalled, wasCalledWith

# Expectations About Calls
@docs Call, calls, hasArgs

# Argument Matchers
@docs Arg, argThat, anyArg, stringArg, intArg, floatArg, boolArg, typedArg, functionArg

-}

import Expect
import Test.Runner
import Elmer exposing (Matcher)
import Elmer.Spy exposing (Calls)
import Elmer.Spy.Arg as Arg
import Elmer.Spy.Call as Call
import Elmer.Printer exposing (..)
import Elmer.Value.Native as Native
import Elmer.Errors as Errors exposing (failWith)


{-| Represents an expected function argument.
-}
type alias Arg =
  Arg.Arg

{-| Represents a particular call to a spy.
-}
type alias Call =
  List Arg

{-| Expect that a spy was called some number of times.

This is shorthand for:

    Spy.expect "my-spy" (calls <| 
      Elmer.hasLength 2
    )

-}
wasCalled : Int -> Matcher Calls
wasCalled expectedCallCount =
  \spy ->
    let
      callCount = List.length spy.calls
    in
      if callCount == expectedCallCount then
        Expect.pass
      else
        failWith <| Errors.wrongNumberOfSpyCalls spy.name expectedCallCount callCount


{-| Matches an argument with the given string.
-}
stringArg : String -> Arg
stringArg =
  Arg.StringArg

{-| Matches an argument with the given integer.
-}
intArg : Int -> Arg
intArg =
  Arg.IntArg

{-| Matches an argument with the given float value.
-}
floatArg : Float -> Arg
floatArg =
  Arg.FloatArg

{-| Matches an argument with the given boolean value.
-}
boolArg : Bool -> Arg
boolArg =
  Arg.BoolArg

{-| Matches an argument with the given typed value.

Note: You may not match what you want if your typed value contains a function reference.
-}
typedArg : a -> Arg
typedArg item =
  Native.cast item
    |> Arg.TypedArg

{-| Matches an argument that is a function.
-}
functionArg : Arg
functionArg =
  Arg.FunctionArg

{-| Matches any argument.
-}
anyArg : Arg
anyArg =
  Arg.AnyArg

{-| Matches an argument whose value satisfies the given matcher.

Suppose you want to expect that the second argument to a function is a record
with a specific value for its name attribute. You could do something like this:

    Spy.expect "my-spy" (
      wasCalledWith
        [ anyArg
        , argThat <|
          \model ->
            model.name
              |> Expect.equal "Awesome Person"
        ]
    )

Note: Bad things will happen if you provide a matcher to `argThat` for a type
that is different from that of the argument you are trying to match.

-}
argThat : Matcher a -> Arg
argThat matcher =
  Native.cast matcher
    |> Arg.ArgThat


{-| Make expectations about the calls recorded by this spy.

Here's how you would expect that exactly 2 of the calls had a certain argument.

    Spy.expect "my-spy" (
      calls <| Elmer.exactly 2 <|
        hasArgs
        [ stringArg "some argument"
        ]
    )

-}
calls : Matcher (List Call) -> Matcher Calls
calls callMatcher =
  \spy ->
    case Test.Runner.getFailureReason <| callMatcher spy.calls of
      Just failure ->
        Expect.fail <|
          format
            [ description <| "Expectation for " ++ spy.name ++ " failed."
            , description <| formatFailure failure
            ]
      Nothing ->
        Expect.pass

{-| Expect that a call has some arguments.

Use `hasArgs` in conjunction with `calls` to make an expectation about the args
of a specific call.

    Spy.expect "my-spy" (
      calls <| Elmer.atIndex 2 <| hasArgs
        [ stringArg "Some String"
        , typedArg someTypedValue
        ]
    )

-}
hasArgs : List Arg -> Matcher Call
hasArgs args =
  \call ->
    evaluateCalls Nothing args [ call ]


{-| Expect that a spy was called at least once with the given arguments.

    Spy.expect "my-spy" (
      wasCalledWith
        [ stringArg "Some String"
        , typedArg someTypedValue
        ]
    )

This is shorthand for:

    Spy.expect "my-spy" (
      calls <| Elmer.some <| hasArgs
        [ stringArg "Some String"
        , typedArg someTypedValue
        ]
    )

-}
wasCalledWith : List Arg -> Matcher Calls
wasCalledWith args =
  \spy ->
    evaluateCalls (Just spy.name) args spy.calls


evaluateCalls : Maybe String -> List Arg -> Matcher (List (List Arg))
evaluateCalls maybeSpyName args =
  \callsArgList ->
    if List.isEmpty callsArgList then
      noCallsFailure maybeSpyName args
    else
      let
        failingCalls =
          callsArgList
            |> List.filter (\callArgs ->
              not <| Call.matches args callArgs
            )
      in
        if List.length failingCalls < List.length callsArgList then
          Expect.pass
        else
          Expect.fail <|
            format <|
              List.append
                [ message (calledWithMessage maybeSpyName) <| Call.asString args
                , message "but it was called with" <| String.join "\n\n" (List.map Call.asString callsArgList)
                ]
                (argThatFailureMessages failingCalls args)


noCallsFailure : Maybe String -> List Arg -> Expect.Expectation
noCallsFailure maybeSpyName args =
  Expect.fail <|
    format
      [ message (calledWithMessage maybeSpyName) <| Call.asString args
      , description "but it was not called"
      ]


argThatFailureMessages : List (List Arg) -> List Arg -> List Message
argThatFailureMessages callsArgList args =
  let
    failureMessages =
      callsArgList
        |> List.map (Call.argThatFailures args)
        |> List.concat
        |> List.map description
  in
    if List.isEmpty failureMessages then
      []
    else
      description "An argThat matcher failed:" ::
        failureMessages


calledWithMessage : Maybe String -> String
calledWithMessage maybeSpyName =
  case maybeSpyName of
    Just spyName ->
      "Expected spy " ++ spyName ++ " to have been called with"
    Nothing ->
      "Expected spy to have been called with"
