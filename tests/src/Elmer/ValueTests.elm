module Elmer.ValueTests exposing (all)

import Test exposing (..)
import Expect
import Elmer exposing (Matcher)
import Elmer.Value as Value
import Json.Decode as Json
import Dict


all : Test
all =
  Test.concat
  [ constructorTests
  , firstArgTests
  , secondArgTests
  , thirdArgTests
  , tupleTests
  , listTests
  , decoderTests
  , dictTests
  ]


constructorTests : Test
constructorTests =
  describe "constructor"
  [ describe "when there are no args for a type"
    [ test "it prints the constructor of the type" <|
      \() ->
        Value.decode Value.constructor NoArgType
          |> expectOk (Expect.equal "NoArgType")
    ]
  , describe "when a type value has args"
    [ test "it prints the constructor of the type" <|
      \() ->
        Value.decode Value.constructor (OneArgType "Hello")
          |> expectOk (Expect.equal "OneArgType")
    ]
  ]


firstArgTests : Test
firstArgTests =
  describe "firstArg"
  [ describe "when there is no arg for a type"
    [ test "it results in an error" <|
      \() ->
        Value.decode (Value.firstArg Json.string) NoArgType
          |> Expect.err
    ]
  , describe "when there is an arg for a type"
    [ test "it maps the first value" <|
      \() ->
        Value.decode (Value.firstArg Json.int) (IntArgType 17)
          |> expectOk (Expect.equal 17)
    ]
  , describe "when there is more than one arg for a type"
    [ test "it maps the first value only" <|
      \() ->
        Value.decode (Value.firstArg Json.int) (TwoIntArgType 17 21)
          |> expectOk (Expect.equal 17)
    ]
  ]


secondArgTests : Test
secondArgTests =
  describe "secondArg"
  [ describe "when there is no arg for a type"
    [ test "it results in an error" <|
      \() ->
        Value.decode (Value.secondArg Json.string) NoArgType
          |> Expect.err
    ]
  , describe "when there is a single arg for a type"
    [ test "it results in an error" <|
      \() ->
        Value.decode (Value.secondArg Json.string) (OneArgType "Fun")
          |> Expect.err
    ]
  , describe "when there are two args for a type"
    [ test "it decondes the second arg" <|
      \() ->
        Value.decode (Value.secondArg Json.int) (TwoArgType "Fun" 17)
          |> expectOk (Expect.equal 17)
    ]
  ]


thirdArgTests : Test
thirdArgTests =
  describe "thirdArg"
  [ describe "when there is not enough args"
    [ test "it results in an error" <|
      \() ->
        Value.decode (Value.thirdArg Json.string) NoArgType
          |> Expect.err
    ]
  , describe "when there are three args for a type"
    [ test "it decodes the third argument" <|
      \() ->
        let
          theType =
            (ThreeArgType "Fun" 17 { first = "Awesome", second = 27 })    
        in
          Value.decode (Value.thirdArg <| Json.field "second" Json.int) theType
            |> expectOk (Expect.equal 27)
    ]
  ]


tupleTests : Test
tupleTests =
  describe "tuple"
  [ describe "when the type is not a tuple"
    [ test "it fails to decode" <|
      \() ->
        Value.decode (Value.tuple Json.string Json.string) NoArgType
          |> Expect.err  
    ]
  , describe "when the type is a tuple"
    [ describe "when the decoders fail"
      [ test "it fails to decode" <|
        \() ->
          Value.decode (Value.tuple Json.int Json.int) ("fun", "awesome")
            |> Expect.err
      ]
    , describe "when the decoders word"
      [ test "it decodes the tuple" <|
        \() ->
          let
            theTuple =
              ("details", { age = 88, sport = "bowling" })
          in
            Value.decode (Value.tuple Json.string (Json.field "sport" Json.string)) theTuple
              |> expectOk (Expect.equal ("details", "bowling"))
      ]
    ]
  ]


listTests : Test
listTests =
  describe "list"
  [ describe "when the value is not a list"
    [ test "it fails to decode" <|
      \() ->
        Value.decode (Value.list Json.string) NoArgType
          |> Expect.err  
    ]
  , describe "when the value is a list"
    [ describe "when the decoder fails"
      [ test "it fails to decode" <|
        \() ->
          Value.decode (Value.list Json.string) [ 7, 34 ]
            |> Expect.err
      ]
    , describe "when the decoder works"
      [ test "it decodes the list" <|
        \() ->
          let
            theList =
              [ { id = 1, age = 8 }, { id = 2, age = 22 } ]
          in
            Value.decode (Value.list <| Json.field "age" Json.int) theList
              |> expectOk (Expect.equal [ 8, 22 ])  
      ]
    ]
  ]


decoderTests : Test
decoderTests =
  describe "decoder"
  [ test "it decodes a non-Json value" <|
    \() ->
      Value.decode (Json.field "f" Value.decoder) { f = \_ -> "You win!" }
        |> expectOk (\f -> f () |> Expect.equal "You win!")
  ]


dictTests : Test
dictTests =
  describe "dict"
  [ test "it decodes a dict" <|
    \() ->
      let
        theDict =
          Dict.fromList
            [ ("name", { item = "brian" })
            , ("age", { item = "17" })
            , ("sports", { item = "bowling" })
            , ("fruit", { item = "apple" })
            ]
      in
        Value.decode (Value.dict <| Json.field "item" Json.string) theDict
          |> expectOk (Elmer.expectAll
              [ Dict.get "name" >> Expect.equal (Just "brian")
              , Dict.get "age" >> Expect.equal (Just "17")
              , Dict.get "sports" >> Expect.equal (Just "bowling")
              , Dict.get "fruit" >> Expect.equal (Just "apple")
              ]
          )
  ]


---


type alias TestTwoArgRecord =
  { first: String
  , second: Int
  }


type TestType
  = NoArgType
  | OneArgType String
  | TwoArgType String Int
  | ThreeArgType String Int TestTwoArgRecord
  | IntArgType Int
  | TwoIntArgType Int Int


expectOk : Matcher a -> Result x a -> Expect.Expectation
expectOk matcher actualResult =
  case actualResult of
    Ok actual ->
      matcher actual
    Err msg ->
      Expect.fail "Expected an Ok value"


expectErr : Matcher x -> Result x a -> Expect.Expectation
expectErr matcher actualResult =
  case actualResult of
    Ok _ ->
      Expect.fail "Expected an Err value"
    Err err ->
      matcher err
