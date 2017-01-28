module Elmer.RuntimeTests exposing (all)

import Test exposing (..)
import Expect

import Html exposing (Html)
import Elmer exposing (..)
import Elmer.Types exposing (..)
import Elmer.TestApps.MessageTestApp as App
import Elmer.Runtime as Runtime
import Elmer.Html.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Command as Command
import Elmer.Html as Markup
import Elmer.Printer exposing (..)
import Task

all : Test
all =
  describe "Runtime Tests"
  [ batchCommandTest
  , batchCommandFailureTest
  , mappedBatchCommandTest
  , unknownCommandTest
  ]

sendFirstMessage : String -> Cmd App.Msg
sendFirstMessage str =
  Command.stub (App.RenderFirstMessage str)

sendSecondMessage : String -> Cmd App.Msg
sendSecondMessage str =
  Command.stub (App.RenderSecondMessage str)

batchCommandTest : Test
batchCommandTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
    batchCommand = Cmd.batch
      [ sendFirstMessage "Cool stuff!"
      , sendSecondMessage "Fun stuff!"
      ]
    result = Command.send batchCommand initialState
  in
    describe "when a batch command is sent"
    [ test "it processes the first command" <|
      \() ->
          Markup.find "#first-message" result
            |> Markup.expectNode (Matchers.hasText "Cool stuff!")
    , test "it processes the second command" <|
      \() ->
          Markup.find "#second-message" result
            |> Markup.expectNode (Matchers.hasText "Fun stuff!")
    ]

batchCommandFailureTest : Test
batchCommandFailureTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
    batchCommand = Cmd.batch
      [ sendFirstMessage "Cool stuff!"
      , Command.fail "It failed!"
      , sendSecondMessage "Fun stuff!"
      ]
    result = Command.send batchCommand initialState
  in
    describe "when a batched command fails"
    [ test "it reports the failure" <|
      \() ->
          Expect.equal (UpstreamFailure "It failed!") result
    ]

mappedBatchCommandTest : Test
mappedBatchCommandTest =
  let
    testModel = { appModel = App.defaultModel }
    initialState = Elmer.componentState testModel parentView parentUpdate
    batchCommand = Cmd.batch
      [ sendFirstMessage "Cool stuff!"
      , sendSecondMessage "Fun stuff!"
      ]
    result = Command.send (Cmd.map AppMsg batchCommand) initialState
  in
    describe "when a batched command is mapped"
    [ test "it maps the first command" <|
      \() ->
          Markup.find "#first-message" result
            |> Markup.expectNode (Matchers.hasText "Cool stuff!")
    , test "it maps the second command" <|
      \() ->
          Markup.find "#second-message" result
            |> Markup.expectNode (Matchers.hasText "Fun stuff!")
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
        initialState = Elmer.componentState App.defaultModel App.view App.update
        unknownCommand = Task.perform App.RenderFirstMessage (Task.succeed "hello")
      in
        Command.send unknownCommand initialState
          |> Expect.equal (UpstreamFailure ( format
            [ message "Elmer encountered a command it does not know how to run" "Task"
            , description "Try sending a stubbed or dummy command instead"
            ]
          ))
  ]
