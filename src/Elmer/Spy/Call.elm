module Elmer.Spy.Call exposing
  ( asString
  , matches
  , argThatFailures
  )

import Test.Runner
import Expect
import Elmer.Spy.Arg as Arg exposing (Arg(..), ArgValue(..))
import Elmer.Printer exposing (..)


matches : List Arg -> List Arg -> Bool
matches expected actual =
  case expected of
    [] ->
      List.isEmpty actual
    x :: xs ->
      case actual of
        [] ->
          False
        y :: ys ->
          case x of
            AnyArg ->
              matches xs ys
            ArgThat matcher ->
              if Expect.pass == (matcher <| Arg.value y) then
                matches xs ys
              else
                False
            arg ->
              if arg == y then
                matches xs ys
              else
                False


argThatFailures : List Arg -> List Arg -> List String
argThatFailures expected actual =
  case expected of
    [] ->
      []
    x :: xs ->
      case actual of
        [] ->
          []
        y :: ys ->
          case x of
            ArgThat matcher ->
              let
                argVal = Arg.value y
              in
                if argVal == FunctionArgValue then
                  "argThat cannot be used to match arguments that are functions" ::
                    argThatFailures xs ys
                else
                  case Test.Runner.getFailureReason <| matcher argVal of
                    Just failure ->
                      formatFailure failure ::
                        argThatFailures xs ys
                    Nothing ->
                      argThatFailures xs ys
            arg ->
              argThatFailures xs ys


asString : List Arg -> String
asString args =
  "[ "
    ++ String.join "\n, " (List.map Arg.asString args)
    ++ "\n]"
