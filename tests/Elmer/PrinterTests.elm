module Elmer.PrinterTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Printer as Printer

all : Test
all =
  describe "Printer tests"
  [ formatMessageTests
  , formatMessageListTests
  ]

formatMessageTests : Test
formatMessageTests =
  describe "formatMessage"
  [ describe "when there is no example"
    [ test "it prints the description" <|
      \() ->
        let
          message = Printer.formatMessage (Printer.description "Fun stuff")
        in
          Expect.equal message "Fun stuff"
    ]
  , describe "when there is an example"
    [ test "it prints the description and the example" <|
      \() ->
        let
          message = Printer.formatMessage (Printer.message "Fun Stuff" "Fun Example")
        in
          Expect.equal message "Fun Stuff\n\n\tFun Example"
    ]
  , describe "when the example has multiple lines"
    [ test "it prints the formatted example" <|
      \() ->
        let
          message = Printer.formatMessage (Printer.message "Fun Stuff" "Fun Example\nSuper Example\nRadical Example\n")
        in
          Expect.equal message "Fun Stuff\n\n\tFun Example\n\tSuper Example\n\tRadical Example"
    ]
  ]

formatMessageListTests : Test
formatMessageListTests =
  describe "format"
  [ test "it prints the messages" <|
    \() ->
      let
        messages = [ Printer.message "Fun Stuff" "Fun Example"
                   , Printer.message "Fun Stuff 2" "Fun Example2"
                   ]
      in
        Expect.equal (Printer.format messages) "Fun Stuff\n\n\tFun Example\n\nFun Stuff 2\n\n\tFun Example2"
  ]
