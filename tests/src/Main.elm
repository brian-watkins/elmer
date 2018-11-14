port module Main exposing (..)

import Platform
import Test exposing (..)
import Expect exposing (Expectation)
import Test.Runner exposing (Runner, SeededRunners(..))
import Test.Runner.Failure as Failure exposing (Reason)
import Random
import Task

import Tests

port sendTestResult : TestResult -> Cmd msg
port sendTestEvent : String -> Cmd msg
port runNextTest : (() -> msg) -> Sub msg

type alias Flags = 
    {}

type alias Model =
    { runners: List Runner
    }

type Msg
    = RunNext ()
    | RunTest Runner

type alias TestResult =
    { descriptions: List String
    , messages: List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seeded = 
            Test.Runner.fromTest 100 (Random.initialSeed 1999) Tests.all
    in    
        ( { runners = runnersFrom seeded }, Cmd.none )


runnersFrom : SeededRunners -> List Runner
runnersFrom seeded =
    case seeded of
        Plain runners ->
            runners
        Only runners ->
            runners
        _ ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunNext _ ->
            case model.runners of
                [] ->
                    ( model, sendTestEvent "DONE" )
                runner :: remaining ->
                    ( { model | runners = remaining }
                    , Task.perform RunTest (Task.succeed runner)
                    )
            
        RunTest runner ->
            ( model
            , sendTestResult <| runTest runner 
            )


runTest : Runner -> TestResult
runTest runner =
    { descriptions = runner.labels
    , messages = 
        runner.run () 
            |> List.filterMap errorMessage
    }


errorMessage : Expectation -> Maybe String
errorMessage expectation =
    case Test.Runner.getFailureReason expectation of
        Just failure ->
            Just <| formatFailure failure
        Nothing ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    runNextTest RunNext


main : Program () Model Msg
main =
    Platform.worker 
        { init = init
        , update = update
        , subscriptions = subscriptions
        }




----

type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Reason
    }

formatFailure : Failure -> String
formatFailure failure =
  case failure.reason of
    Failure.Custom ->
      failure.description
    Failure.Equality one two ->
      case failure.description of
        "Expect.equal" ->
          one ++ " is not equal to " ++ two
        "Expect.notEqual" ->
          one ++ " is equal to " ++ two
        otherEquality ->
          otherEquality ++ " failed between " ++ two ++ " and " ++ one
    Failure.Comparison one two ->
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
    Failure.ListDiff one two ->
      formatList two
        ++ "\n\nis not equal to\n\n"
        ++ formatList one
    Failure.CollectionDiff data ->
      "Expected\n\n"
        ++ data.expected
        ++ "\n\nbut the actual value is\n\n"
        ++ data.actual
    _ ->
      "Failure " ++ failureToString failure

failureToString : Failure -> String
failureToString reason =
  reason.description

formatList : List String -> String
formatList list =
  "[ " ++ (String.join ", " list) ++ " ]"
