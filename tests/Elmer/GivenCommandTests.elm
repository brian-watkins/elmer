module Elmer.GivenCommandTests exposing (..)

import Test exposing (..)
import Expect
import Elmer exposing (exactly)
import Elmer.Http exposing (HttpResponseStub)
import Elmer.Http.Route exposing (..)
import Elmer.Http.Stub as Stub exposing (withBody)
import Elmer.Spy as Spy
import Http

type Msg
  = HttpRequest (Result Http.Error String)

httpCommand : String -> Cmd Msg
httpCommand url =
  Http.getString url
    |> Http.send HttpRequest

httpResponse : String -> HttpResponseStub
httpResponse url =
  Stub.for (get url)
    |> withBody "Hey!"

spyTests : Test
spyTests =
  describe "when a given command triggers a spy"
  [ test "the expectation is satisfied" <|
    \() ->
      Elmer.givenCommand (\() -> httpCommand "http://fake.com/my-fake-stuff")
        |> Spy.use [ Elmer.Http.spy ]
        |> Elmer.Http.expect (get "http://fake.com/my-fake-stuff")
  ]

expectMessageTests : Test
expectMessageTests =
  describe "when a given command results in a message"
  [ test "it records the messages" <|
    \() ->
      Elmer.givenCommand (\() -> httpCommand "http://fake.com/my-fake-stuff")
        |> Spy.use [ Elmer.Http.serve [ httpResponse "http://fake.com/my-fake-stuff" ] ]
        |> Elmer.expectMessages (
          exactly 1 <| Expect.equal (HttpRequest <| Ok "Hey!")
        )
  ]
