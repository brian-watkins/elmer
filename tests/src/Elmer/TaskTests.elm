module Elmer.TaskTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Command as Command
import Elmer.Task
import Task
import Time exposing (Posix)


all : Test
all =
  Test.concat
  [ failTestTaskTests
  , realTaskTests
  , andThenTests
  , sequenceTests
  , mapTests
  , onErrorTests
  , mapErrorTests
  ]

type TestTagger
  = TagInt Int
  | TagString String
  | TagList (List Int)
  | TagResult (Result String Int)
  | TagResultString (Result String String)
  | TagTestData TestData
  | TagTestData3 TestData3
  | TagTestData4 TestData4
  | TagTestData5 TestData5
  | TagTime Posix


type alias TestData =
  { name : String
  , age : Int
  }

type alias TestData3 =
  { name : String
  , age : Int
  , thing : String
  }

type alias TestData4 =
  { name : String
  , age : Int
  , thing : String
  , weather : String
  }

type alias TestData5 =
  { name : String
  , age : Int
  , thing : String
  , weather : String
  , fruit : String
  }

failTestTaskTests : Test
failTestTaskTests =
  describe "failTest"
  [ test "it fails the test with the given message" <|
    \() ->
      Command.given (\_ ->
        Elmer.Task.failTest "It failed!"
          |> Task.attempt TagResult
      )
      |> Command.expectMessages (Expect.equal [])
      |> Expect.equal (Expect.fail "It failed!")
  ]


realTaskTests : Test
realTaskTests =
  describe "real tasks"
  [ test "it handles a real succeed task" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 37
          |> Task.perform TagInt
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagInt 37
        )
      )
  , test "it handles a real failed task" <|
    \() ->
      Command.given (\_ ->
        Task.fail "Failed"
          |> Task.attempt TagResult
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResult <| Err "Failed"
        )
      )
  , test "it fails with an unknown task" <|
    \() ->
      Command.given (\_ ->
        Time.now
          |> Task.perform TagTime
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagTime <| Time.millisToPosix 100
        )
      )
      |> Expect.equal (Expect.fail "Encountered a native task.\nStub any task-generating functions with Task.succeed or Task.fail as necessary.")
  ]


andThenTests : Test
andThenTests =
  describe "andThen"
  [ test "it handles tasks connected with andThen" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 27
          |> Task.andThen (\i -> Task.succeed <| i + 13)
          |> Task.andThen (\i -> Task.succeed <| "Hello " ++ (String.fromInt i))
          |> Task.perform TagString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagString "Hello 40"
        )
      )
  , test "it fails with the first task fails" <|
    \() ->
      Command.given (\_ ->
        Task.fail "Failed!"
          |> Task.andThen (\i -> Task.succeed <| i + 13)
          |> Task.attempt TagResult
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResult <| Err "Failed!"
        )
      )
  ]


sequenceTests : Test
sequenceTests =
  describe "sequence"
  [ test "it handles a sequence of tasks" <|
    \() ->
      Command.given (\_ ->
        [ Task.succeed 27
        , Task.succeed 19
        , Task.succeed 31
        , Task.succeed 8
        ]
          |> Task.sequence
          |> Task.perform TagList
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagList [ 27, 19, 31, 8 ]
        )
      )
  ]


mapTests : Test
mapTests =
  describe "map"
  [ test "it handles a mapped task" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 27
          |> Task.map (\i -> i + 81)
          |> Task.map String.fromInt
          |> Task.perform TagString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagString "108"
        )
      )
  , test "it handles a mapped andThen task" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 27
          |> Task.andThen (\num -> Task.succeed <| num + 100)
          |> Task.map (\i -> i + 81)
          |> Task.map String.fromInt
          |> Task.perform TagString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagString "208"
        )
      )
  , test "it does not map if it fails" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 27
          |> Task.andThen (\num -> Task.fail <| "Failed with " ++ String.fromInt num)
          |> Task.map (\i -> i + 81)
          |> Task.map String.fromInt
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Err "Failed with 27"
        )
      )
  , test "it handles two mapped tasks" <|
    \() ->
      Command.given (\_ ->
        Task.map2 TestData
          (Task.succeed "Awesome Dude")
          (Task.succeed 27)
          |> Task.perform TagTestData
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagTestData
            { name = "Awesome Dude"
            , age = 27
            }
        )
      )
  , test "it handles three mapped tasks" <|
    \() ->
      Command.given (\_ ->
        Task.map3 TestData3
          (Task.succeed "Awesome Dude")
          (Task.succeed 27)
          (Task.succeed "Fun")
          |> Task.perform TagTestData3
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagTestData3
            { name = "Awesome Dude"
            , age = 27
            , thing = "Fun"
            }
        )
      )
  , test "it handles four mapped tasks" <|
    \() ->
      Command.given (\_ ->
        Task.map4 TestData4
          (Task.succeed "Awesome Dude")
          (Task.succeed 27)
          (Task.succeed "Fun")
          (Task.succeed "Snow")
          |> Task.perform TagTestData4
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagTestData4
            { name = "Awesome Dude"
            , age = 27
            , thing = "Fun"
            , weather = "Snow"
            }
        )
      )
  , test "it handles five mapped tasks" <|
    \() ->
      Command.given (\_ ->
        Task.map5 TestData5
          (Task.succeed "Awesome Dude")
          (Task.succeed 27)
          (Task.succeed "Fun")
          (Task.succeed "Snow")
          (Task.succeed "Apple")
          |> Task.perform TagTestData5
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagTestData5
            { name = "Awesome Dude"
            , age = 27
            , thing = "Fun"
            , weather = "Snow"
            , fruit = "Apple"
            }
        )
      )
  ]

onErrorTests : Test
onErrorTests =
  describe "onError"
  [ test "it handles onError with a simple successful task" <|
    \() ->
      Command.given (\_ ->
        Task.fail "I failed"
          |> Task.onError (\error ->
            "Recovered from error: " ++ error
              |> Task.succeed
            )
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Ok "Recovered from error: I failed"
        )
      )
  , test "it handles onError with a more complex successful task" <|
    \() ->
      Command.given (\_ ->
        Task.fail "I failed"
          |> Task.onError (\error ->
            "Recovered from error: " ++ error
              |> Task.succeed
              |> Task.andThen (\msg -> Task.succeed <| "A message: " ++ msg)
            )
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Ok "A message: Recovered from error: I failed"
        )
      )
  , test "it handles onError with a simple failed task" <|
    \() ->
      Command.given (\_ ->
        Task.fail "I failed"
          |> Task.onError (\error ->
            "I also failed after: " ++ error
              |> Task.fail
            )
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Err "I also failed after: I failed"
        )
      )
  , test "it handles onError applied to an andThen task" <|
      \() ->
        Command.given (\_ ->
          Task.succeed 27
            |> Task.andThen (\i -> Task.fail <| "Bad number => " ++ String.fromInt i)
            |> Task.onError (\err -> Task.succeed 15)
            |> Task.attempt TagResult
        )
        |> Command.expectMessages (
          exactly 1 <| Expect.equal (
            TagResult <| Ok 15
          )
        )
  , test "it handles onError applied to a sequence task" <|
    \() ->
      Command.given (\_ ->
        [ Task.succeed 27
        , Task.succeed 19
        , Task.fail "Failed"
        , Task.succeed 8
        ]
          |> Task.sequence
          |> Task.onError (\msg -> Task.succeed [99])
          |> Task.perform TagList
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagList [ 99 ]
        )
      )
  ]


mapErrorTests : Test
mapErrorTests =
  describe "mapError"
  [ test "it maps the error value" <|
    \() ->
      Command.given (\_ ->
        Task.fail "I failed"
          |> Task.mapError (\error ->
            "Mapped error: " ++ error
          )
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Err "Mapped error: I failed"
        )
      )
  , test "it does not map a success" <|
    \() ->
      Command.given (\_ ->
        Task.succeed "Success!"
          |> Task.mapError (\error ->
            "Mapped error: " ++ error
          )
          |> Task.attempt TagResultString
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Ok "Success!"
        )
      )
  , test "it maps the error value for an andThen task" <|
    \() ->
      Command.given (\_ ->
        Task.succeed 27
          |> Task.andThen (\i -> Task.fail <| "Bad number => " ++ String.fromInt i)
          |> Task.mapError (\err -> "Got an error: " ++ err)
          |> Task.attempt TagResult
      )
      |> Command.expectMessages (
        exactly 1 <| Expect.equal (
          TagResult <| Err "Got an error: Bad number => 27"
        )
      )
  ]
