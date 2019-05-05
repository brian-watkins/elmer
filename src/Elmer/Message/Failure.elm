module Elmer.Message.Failure exposing
  ( Failure
  , format
  )

{-| Format a test failure reason from
the [elm-explorations/test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
package.

Note that these functions are mainly useful when writing extensions to Elmer
or custom matchers.

@docs Failure, format

-}

import Test.Runner.Failure

{-| Represents a failure reason from the
[elm-explorations/test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) package.

You can obtain a failure reason with `Test.Runner.getFailureReason`.

See [the elm-explorations/test docs](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Runner#getFailureReason)
for more information.

-}
type alias Failure =
  { given: Maybe String
  , description: String
  , reason: Test.Runner.Failure.Reason
  }


{-| Format a list of `Failure` values as a string.
-}
format : List Failure -> String
format failures =
  List.map formatFailure failures
    |> joinMessages


formatFailure : Failure -> String
formatFailure failure =
  case failure.reason of
    Test.Runner.Failure.Custom ->
      failure.description
    Test.Runner.Failure.Equality one two ->
      case failure.description of
        "Expect.equal" ->
          one ++ " is not equal to " ++ two
        "Expect.notEqual" ->
          one ++ " is equal to " ++ two
        otherEquality ->
          otherEquality ++ " failed between " ++ two ++ " and " ++ one
    Test.Runner.Failure.Comparison one two ->
      case failure.description of
        "Expect.lessThan" ->
          two ++ " is not less than " ++ one
        "Expect.atMost" ->
          two ++ " is not at most " ++ one
        "Expect.greaterThan" ->
          two ++ " is not greater than " ++ one
        "Expect.atLeast" ->
          two ++ " is not at least " ++ one
        "Expect.err" ->
          two ++ " is not an Err"
        otherComparison ->
          otherComparison ++ " failed between " ++ two ++ " and " ++ one
    Test.Runner.Failure.ListDiff one two ->
      formatList two
        ++ "\n\nis not equal to\n\n"
        ++ formatList one
    Test.Runner.Failure.CollectionDiff data ->
      "Expected\n\n"
        ++ data.expected
        ++ "\n\nbut the actual value is\n\n"
        ++ data.actual
    _ ->
      "Failure " ++ Debug.toString failure


failureToString : Failure -> String
failureToString reason =
  reason.description


formatList : List String -> String
formatList list =
  "[ " ++ (String.join ", " list) ++ " ]"


joinMessages : List String -> String
joinMessages =
  String.join "\n\n"
