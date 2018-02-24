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
              case Arg.value y of
                Nothing ->
                  False
                Just val ->
                  if Expect.pass == (matcher val) then
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
              case Arg.value y of
                Nothing ->
                  "argThat cannot be used to match arguments that are functions" ::
                    argThatFailures xs ys
                Just val ->
                  case Test.Runner.getFailureReason <| matcher val of
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
