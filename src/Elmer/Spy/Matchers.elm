module Elmer.Spy.Matchers exposing
  ( Arg
  , stringArg
  , intArg
  , floatArg
  , boolArg
  , typedArg
  , anyArg
  , wasCalled
  , wasCalledWith
  )

{-| Matchers for making expectations about Spies

@docs wasCalled, wasCalledWith

# Argument Matchers
@docs Arg, stringArg, intArg, floatArg, boolArg, typedArg, anyArg

-}

import Expect
import Elmer exposing (Matcher)
import Elmer.Internal as Internal
import Elmer.Spy exposing (Calls)
import Elmer.Spy.Internal as Spy_
import Elmer.Printer exposing (..)

{-| Represents an expected function argument.
-}
type alias Arg =
  Spy_.Arg

{-| Expect that a spy was called some number of times.
-}
wasCalled : Int -> Matcher Calls
wasCalled times spy =
  let
    calls = List.length spy.calls
  in
    if calls == times then
      Expect.pass
    else
      Expect.fail <|
        format
          [ message ("Expected spy " ++ spy.name ++ " to have been called") <| timesString times
          , message "but it was called" <| timesString calls
          ]

timesString : Int -> String
timesString times =
  if times == 1 then
    (toString times) ++ " time"
  else
    (toString times) ++ " times"


{-| Matches an argument with the given string.
-}
stringArg : String -> Arg
stringArg =
  Spy_.StringArg

{-| Matches an argument with the given integer.
-}
intArg : Int -> Arg
intArg =
  Spy_.IntArg

{-| Matches an argument with the given float value.
-}
floatArg : Float -> Arg
floatArg =
  Spy_.FloatArg

{-| Matches an argument with the given boolean value.
-}
boolArg : Bool -> Arg
boolArg =
  Spy_.BoolArg

{-| Matches an argument with the given typed value.

Note: You may not match what you want if your typed value contains a function reference.
-}
typedArg : a -> Arg
typedArg item =
  Spy_.TypedArg (toString item)

{-| Matches any argument.
-}
anyArg : Arg
anyArg =
  Spy_.AnyArg


{-| Expect that a spy was called at least once with the given arguments.

    Spy.expect "my-spy" (
      wasCalledWith
        [ stringArg "Some String"
        , typedArg someTypedValue
        ]
    )
-}
wasCalledWith : List Arg -> Matcher Calls
wasCalledWith args spy =
  if List.isEmpty spy.calls then
    Expect.fail <|
      format
        [ message ("Expected spy " ++ spy.name ++ " to have been called with") <| argsString args
        , description "but it was not called"
        ]
  else
    let
      matchingCalls =
        spy.calls
          |> List.filter (\callArgs ->
            argListMatches args callArgs
          )
    in
      if List.isEmpty matchingCalls then
        Expect.fail <|
          format
            [ message ("Expected spy " ++ spy.name ++ " to have been called with") <| argsString args
            , message "but it was called with" <| String.join "\n\n" (List.map argsString spy.calls)
            ]
      else
        Expect.pass

argListMatches : List Arg -> List Arg -> Bool
argListMatches expected actual =
  case expected of
    [] ->
      List.isEmpty actual
    x :: xs ->
      case actual of
        [] ->
          False
        y :: ys ->
          if x == Spy_.AnyArg || x == y then
            argListMatches xs ys
          else
            False

argsString : List Arg -> String
argsString args =
  "[ "
    ++ String.join "\n, " (List.map printArg args)
    ++ "\n]"

printArg : Spy_.Arg -> String
printArg arg =
  case arg of
    Spy_.StringArg str ->
      "\"" ++ str ++ "\""
    Spy_.IntArg num ->
      toString num
    Spy_.FloatArg num ->
      toString num
    Spy_.BoolArg bool ->
       Internal.boolToString bool
    Spy_.TypedArg str ->
      str
    Spy_.AnyArg ->
      "(ANY)"
