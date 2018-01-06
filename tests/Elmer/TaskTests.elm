module Elmer.TaskTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Headless as Headless
import Elmer.Task exposing (TaskResult(..))
import Task
import Time exposing (Time)

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
  | TagTime Time


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

realTaskTests : Test
realTaskTests =
  describe "real tasks"
  [ test "it handles a real succeed task" <|
    \() ->
      Headless.givenCommand (\_ ->
        Task.succeed 37
          |> Task.perform TagInt
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagInt 37
        )
      )
  , test "it handles a real failed task" <|
    \() ->
      Headless.givenCommand (\_ ->
        Task.fail "Failed"
          |> Task.attempt TagResult
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResult <| Err "Failed"
        )
      )
  , test "it fails with an unknown task" <|
    \() ->
      Headless.givenCommand (\_ ->
        Time.now
          |> Task.perform TagTime
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagTime 100
        )
      )
      |> Expect.equal (Expect.fail "Encountered a real task. Use Elmer.Task.fake to stub any task-generating functions.")
  ]

fakeTaskTests : Test
fakeTaskTests =
  describe "fake"
  [ test "it creates a task that succeeds with the message" <|
    \() ->
      Headless.givenCommand (\_ ->
        Success 27
          |> Elmer.Task.fake
          |> Task.perform TagInt
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagInt 27
        )
      )
  , test "it creates a task that fails with the message" <|
    \() ->
      Headless.givenCommand (\_ ->
        Failure "You Failed!"
          |> Elmer.Task.fake
          |> Task.attempt TagResult
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResult <| Err "You Failed!"
        )
      )
  ]

andThenTests : Test
andThenTests =
  describe "andThen"
  [ test "it handles tasks connected with andThen" <|
    \() ->
      Headless.givenCommand (\_ ->
        Success 27
          |> Elmer.Task.fake
          |> Task.andThen (\i -> Elmer.Task.fake <| Success (i + 13))
          |> Task.andThen (\i -> Elmer.Task.fake <| Success ("Hello " ++ (toString i)))
          |> Task.perform TagString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagString "Hello 40"
        )
      )
  ]


sequenceTests : Test
sequenceTests =
  describe "sequence"
  [ test "it handles a sequence of tasks" <|
    \() ->
      Headless.givenCommand (\_ ->
        [ Elmer.Task.fake <| Success 27
        , Elmer.Task.fake <| Success 19
        , Elmer.Task.fake <| Success 31
        , Elmer.Task.fake <| Success 8
        ]
          |> Task.sequence
          |> Task.perform TagList
      )
      |> Headless.expectMessages (
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
      Headless.givenCommand (\_ ->
        Success 27
          |> Elmer.Task.fake
          |> Task.map (\i -> i + 81)
          |> Task.map toString
          |> Task.perform TagString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagString "108"
        )
      )
  , test "it handles two mapped tasks" <|
    \() ->
      Headless.givenCommand (\_ ->
        Task.map2 TestData
          (Elmer.Task.fake <| Success "Awesome Dude")
          (Elmer.Task.fake <| Success 27)
          |> Task.perform TagTestData
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagTestData
            { name = "Awesome Dude"
            , age = 27
            }
        )
      )
  , test "it handles three mapped tasks" <|
    \() ->
      Headless.givenCommand (\_ ->
        Task.map3 TestData3
          (Elmer.Task.fake <| Success "Awesome Dude")
          (Elmer.Task.fake <| Success 27)
          (Elmer.Task.fake <| Success "Fun")
          |> Task.perform TagTestData3
      )
      |> Headless.expectMessages (
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
      Headless.givenCommand (\_ ->
        Task.map4 TestData4
          (Elmer.Task.fake <| Success "Awesome Dude")
          (Elmer.Task.fake <| Success 27)
          (Elmer.Task.fake <| Success "Fun")
          (Elmer.Task.fake <| Success "Snow")
          |> Task.perform TagTestData4
      )
      |> Headless.expectMessages (
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
      Headless.givenCommand (\_ ->
        Task.map5 TestData5
          (Elmer.Task.fake <| Success "Awesome Dude")
          (Elmer.Task.fake <| Success 27)
          (Elmer.Task.fake <| Success "Fun")
          (Elmer.Task.fake <| Success "Snow")
          (Elmer.Task.fake <| Success "Apple")
          |> Task.perform TagTestData5
      )
      |> Headless.expectMessages (
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
      Headless.givenCommand (\_ ->
        Failure "I failed"
          |> Elmer.Task.fake
          |> Task.onError (\error ->
            "Recovered from error: " ++ error
              |> Success
              |> Elmer.Task.fake
            )
          |> Task.attempt TagResultString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Ok "Recovered from error: I failed"
        )
      )
  , test "it handles onError with a more complex successful task" <|
    \() ->
      Headless.givenCommand (\_ ->
        Failure "I failed"
          |> Elmer.Task.fake
          |> Task.onError (\error ->
            "Recovered from error: " ++ error
              |> Success
              |> Elmer.Task.fake
              |> Task.andThen (\msg -> Elmer.Task.fake <| Success <| "A message: " ++ msg)
            )
          |> Task.attempt TagResultString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Ok "A message: Recovered from error: I failed"
        )
      )
  , test "it handles onError with a simple failed task" <|
    \() ->
      Headless.givenCommand (\_ ->
        Failure "I failed"
          |> Elmer.Task.fake
          |> Task.onError (\error ->
            "I also failed after: " ++ error
              |> Failure
              |> Elmer.Task.fake
            )
          |> Task.attempt TagResultString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Err "I also failed after: I failed"
        )
      )
  ]


mapErrorTests : Test
mapErrorTests =
  describe "mapError"
  [ test "it maps the error value" <|
    \() ->
      Headless.givenCommand (\_ ->
        Failure "I failed"
          |> Elmer.Task.fake
          |> Task.mapError (\error ->
            "Mapped error: " ++ error
          )
          |> Task.attempt TagResultString
      )
      |> Headless.expectMessages (
        exactly 1 <| Expect.equal (
          TagResultString <| Err "Mapped error: I failed"
        )
      )
  ]
