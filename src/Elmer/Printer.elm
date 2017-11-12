module Elmer.Printer exposing
  ( Message
  , FailureReason
  , description
  , message
  , formatMessage
  , format
  , formatFailures
  , formatFailure
  )

import Test.Runner.Failure as ElmTestFailure

type alias Message =
  { description: String
  , example: Maybe String
  }

type alias FailureReason =
  { given: Maybe String
  , description: String
  , reason: ElmTestFailure.Reason
  }

description : String -> Message
description description =
  { description = description
  , example = Nothing
  }

message : String -> String -> Message
message description example =
  { description = description
  , example = Just example
  }

formatMessage : Message -> String
formatMessage message =
  case message.example of
    Just example ->
      message.description ++ "\n\n" ++ (formatExample example)
    Nothing ->
      message.description

formatExample : String -> String
formatExample example =
  String.split "\n" example
    |> List.foldl (\s msg -> msg ++ "\t" ++ s ++ "\n") ""
    |> String.trimRight

format : List Message -> String
format messages =
  List.map formatMessage messages
    |> joinMessages

joinMessages : List String -> String
joinMessages =
  String.join "\n\n"

formatFailures : List FailureReason -> String
formatFailures failures =
  List.map formatFailure failures
    |> joinMessages

formatFailure : FailureReason -> String
formatFailure failure =
  case failure.reason of
    ElmTestFailure.Custom ->
      failure.description
    ElmTestFailure.Equality one two ->
      case failure.description of
        "Expect.equal" ->
          one ++ " is not equal to " ++ two
        "Expect.notEqual" ->
          one ++ " is equal to " ++ two
        otherEquality ->
          otherEquality ++ " failed between " ++ two ++ " and " ++ one
    ElmTestFailure.Comparison one two ->
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
    ElmTestFailure.ListDiff one two ->
      formatList two
        ++ "\n\nis not equal to\n\n"
        ++ formatList one
    ElmTestFailure.CollectionDiff data ->
      "Expected\n\n"
        ++ data.expected
        ++ "\n\nbut the actual value is\n\n"
        ++ data.actual
    _ ->
      "Failure " ++ toString failure

formatList : List String -> String
formatList list =
  "[ " ++ (String.join ", " list) ++ " ]"
