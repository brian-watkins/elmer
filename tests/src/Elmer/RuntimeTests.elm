module Elmer.RuntimeTests exposing (..)

import Test exposing (..)
import Expect

import Html exposing (Html)
import Elmer exposing (..)
import Elmer.TestState as TestState exposing (TestState)
import Elmer.TestApps.MessageTestApp as App
import Elmer.Runtime as Runtime
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Html.Selector as Sel exposing (..)
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Platform.Command as Command
import Elmer.Html as Markup
import Elmer.Printer exposing (..)
import Task
import Time


all : Test
all =
  Test.concat
  [ batchCommandTest
  , batchCommandFailureTest
  , mappedBatchCommandTest
  , unknownCommandTest
  ]


sendFirstMessage : String -> Cmd App.Msg
sendFirstMessage str =
  Command.fake (App.RenderFirstMessage str)

sendSecondMessage : String -> Cmd App.Msg
sendSecondMessage str =
  Command.fake (App.RenderSecondMessage str)

batchCommandTest : Test
batchCommandTest =
  let
    initialState = Elmer.given App.defaultModel App.view App.update
    batchCommandThunk = \() ->
      Cmd.batch
        [ sendFirstMessage "Cool stuff!"
        , sendSecondMessage "Fun stuff!"
        ]
    result = Command.send batchCommandThunk initialState
  in
    describe "when a batch command is sent"
    [ test "it processes the first command" <|
      \() ->
        result
          |> Markup.target << by [ id "first-message" ]
          |> Markup.expect (element <| hasText "Cool stuff!")
    , test "it processes the second command" <|
      \() ->
        result
          |> Markup.target << by [ id "second-message" ]
          |> Markup.expect (element <| hasText "Fun stuff!")
    ]

batchCommandFailureTest : Test
batchCommandFailureTest =
  let
    initialState = Elmer.given App.defaultModel App.view App.update
    batchCommandThunk = \() ->
      Cmd.batch
        [ sendFirstMessage "Cool stuff!"
        , Command.fail "It failed!"
        , sendSecondMessage "Fun stuff!"
        ]
    result = Command.send batchCommandThunk initialState
  in
    describe "when a batched command fails"
    [ test "it reports the failure" <|
      \() ->
          Expect.equal (TestState.failure "It failed!") result
    ]

mappedBatchCommandTest : Test
mappedBatchCommandTest =
  let
    testModel = { appModel = App.defaultModel }
    initialState = Elmer.given testModel parentView parentUpdate
    batchCommand = Cmd.batch
      [ sendFirstMessage "Cool stuff!"
      , sendSecondMessage "Fun stuff!"
      ]
    result = Command.send (\() -> Cmd.map AppMsg batchCommand) initialState
  in
    describe "when a batched command is mapped"
    [ test "it maps the first command" <|
      \() ->
        result
          |> Markup.target << by [ id "first-message" ]
          |> Markup.expect (element <| hasText "Cool stuff!")
    , test "it maps the second command" <|
      \() ->
        result
          |> Markup.target << by [ id "second-message" ]
          |> Markup.expect (element <| hasText "Fun stuff!")
    ]

type ParentMsg
  = AppMsg App.Msg

type alias ParentModel =
  { appModel : App.Model }

parentView : ParentModel -> Html ParentMsg
parentView parentModel =
  Html.map AppMsg (App.view parentModel.appModel)

parentUpdate : ParentMsg -> ParentModel -> ( ParentModel, Cmd ParentMsg )
parentUpdate parentMsg model =
  case parentMsg of
    AppMsg appMsg ->
      let
        ( updatedAppModel, updatedAppCmd ) = App.update appMsg model.appModel
        updatedModel = { model | appModel = updatedAppModel }
        updatedCmd = Cmd.map AppMsg updatedAppCmd
      in
        ( updatedModel, updatedCmd )


unknownCommandTest : Test
unknownCommandTest =
  describe "when the runtime receives an unknown command"
  [ test "it fails" <|
    \() ->
      let
        initialState = Elmer.given App.defaultModel App.view App.update
        unknownCommandThunk = \() -> Task.perform App.RenderFirstMessage (Time.now |> Task.map (Time.posixToMillis >> String.fromInt))
      in
        Command.send unknownCommandThunk initialState
          |> Expect.equal (TestState.failure ( format
            [ description "Encountered a native task.\nStub any task-generating functions with Task.succeed or Task.fail as necessary."
            ]
          ))
  ]
