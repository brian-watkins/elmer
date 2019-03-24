module Elmer.SpySpanTests exposing (..)

import Test exposing (..)
import Expect
import Elmer
import Elmer.Spy as Spy exposing (Spy, andCallThrough)
import Elmer.Spy.Matchers exposing (..)
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Html.Event as Event
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


all : Test
all =
  Test.concat
  [ spanTests
  ]


spanTests : Test
spanTests =
  describe "when calls to a spy span multiple test state steps" <|
  let
    testState =
      Elmer.given defaultModel view update
        |> Spy.use [ multiStepFunctionSpy ]
        |> Markup.target << by [ id "input-field" ]
        |> Event.input "Some text"
        |> Markup.target << by [ id "submit-button" ]
        |> Event.click
  in
  [ test "it works as expected" <|
    \() ->
      testState
        |> Markup.target << by [ id "result" ]
        |> Markup.expect (element <| hasText "Some text AND 17")
  , test "it records all the calls" <|
    \() ->
      testState  
        |> Spy.expect (\_ -> multiStepFunction) (
          Elmer.expectAll
          [ wasCalledWith [ stringArg "Some text", intArg 17 ]
          ]
        )
  ]


multiStepFunctionSpy : Spy
multiStepFunctionSpy =
  Spy.observe (\_ -> multiStepFunction)
    |> andCallThrough


multiStepFunction : String -> Int -> String
multiStepFunction word num =
  word ++ " AND " ++ String.fromInt num


type Msg
  = GotInput String
  | ClickedButton

type alias Model = 
  { text: String
  , count: Int
  , someFunc: (Int -> String)
  }

defaultModel : Model
defaultModel =
  { text = ""
  , count = 17
  , someFunc = \_ -> ""
  }

view : Model -> Html Msg
view model =
  Html.div []
  [ Html.div [ Attr.id "result" ]
    [ Html.text <| model.text ]
  , Html.input [ Attr.id "input-field", Events.onInput GotInput ]
    []
  , Html.button [ Attr.id "submit-button", Events.onClick ClickedButton ]
    [ Html.text "Click me!" ]
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotInput val ->
      ({ model | text = val, someFunc = multiStepFunction val }, Cmd.none)
    ClickedButton ->
      ({ model | text = model.someFunc model.count }, Cmd.none)
