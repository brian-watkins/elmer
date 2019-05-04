module Elmer.MessageTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Message as Message


all : Test
all =
  Test.concat
  [ formatTests
  ]


formatTests : Test
formatTests =
  describe "Message.format"
  [ describe "when there is no example"
    [ test "it prints the description" <|
      \() ->
        let
          message = 
            [ Message.note "Fun stuff" ]
              |> Message.format
        in
          Expect.equal message "Fun stuff"
    ]
  , describe "when there is an example"
    [ test "it prints the description and the example" <|
      \() ->
        let
          message = 
            [ Message.fact "Fun Stuff" "Fun Example" ]
              |> Message.format
        in
          Expect.equal message "Fun Stuff\n\n\tFun Example"
    ]
  , describe "when the example has multiple lines"
    [ test "it prints the formatted example" <|
      \() ->
        let
          message = 
            [ Message.fact "Fun Stuff" "Fun Example\nSuper Example\nRadical Example\n" ]
              |> Message.format
        in
          Expect.equal message "Fun Stuff\n\n\tFun Example\n\tSuper Example\n\tRadical Example"
    ]
  , describe "when there are multiple messages"
    [ test "it prints all the messages" <|
      \() ->
        let
          messages = 
            [ Message.fact "Fun Stuff" "Fun Example"
            , Message.fact "Fun Stuff 2" "Fun Example2"
            ]
        in
          Message.format messages
            |> Expect.equal "Fun Stuff\n\n\tFun Example\n\nFun Stuff 2\n\n\tFun Example2"
    ]
  ]

