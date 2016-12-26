module Elmer.RuntimeTests exposing (all)

import Test exposing (..)
import Expect

import Html exposing (Html)
import Elmer exposing (..)
import Elmer.Types exposing (..)
import Elmer.Event as Event
import Elmer.TestApps.TaskTestApp as App
import Elmer.Runtime as Runtime
import Elmer.Matchers as Matchers
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Command as Command

import Task

all : Test
all =
  describe "Runtime Tests"
  [ batchCommandTest
  , batchCommandFailureTest
  , mappedBatchCommandTest
  ]


batchCommandTest : Test
batchCommandTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
    batchCommand = Cmd.batch
      [ App.sendFirstTask "Cool stuff!"
      , App.sendSecondTask "Fun stuff!"
      ]
    result = Event.sendCommand batchCommand initialState
  in
    describe "when a batch command is sent"
    [ test "it processes the first command" <|
      \() ->
          Elmer.find "#first-task-result" result
            |> Elmer.expectNode (Matchers.hasText "Cool stuff!")
    , test "it processes the second command" <|
      \() ->
          Elmer.find "#second-task-result" result
            |> Elmer.expectNode (Matchers.hasText "Fun stuff!")
    ]

batchCommandFailureTest : Test
batchCommandFailureTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
    batchCommand = Cmd.batch
      [ App.sendFirstTask "Cool stuff!"
      , Command.failureCommand "It failed!"
      , App.sendSecondTask "Fun stuff!"
      ]
    result = Event.sendCommand batchCommand initialState
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
      [ App.sendFirstTask "Cool stuff!"
      , App.sendSecondTask "Fun stuff!"
      ]
    result = Event.sendCommand (Cmd.map AppMsg batchCommand) initialState
  in
    describe "when a batched command is mapped"
    [ test "it maps the first command" <|
      \() ->
          Elmer.find "#first-task-result" result
            |> Elmer.expectNode (Matchers.hasText "Cool stuff!")
    , test "it maps the second command" <|
      \() ->
          Elmer.find "#second-task-result" result
            |> Elmer.expectNode (Matchers.hasText "Fun stuff!")
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
