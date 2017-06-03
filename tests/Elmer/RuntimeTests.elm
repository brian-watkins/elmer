module Elmer.RuntimeTests exposing (..)

import Test exposing (..)
import Expect

import Html exposing (Html)
import Elmer exposing (..)
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.TestApps.MessageTestApp as App
import Elmer.Runtime as Runtime
import Elmer.Html.Matchers as Matchers exposing (element, hasText)
import Elmer.Http as ElmerHttp
import Elmer.Http.Stub as HttpStub
import Elmer.Platform.Command as Command
import Elmer.Html as Markup
import Elmer.Printer exposing (..)
import Task


sendFirstMessage : String -> Cmd App.Msg
sendFirstMessage str =
  Command.fake (App.RenderFirstMessage str)

sendSecondMessage : String -> Cmd App.Msg
sendSecondMessage str =
  Command.fake (App.RenderSecondMessage str)

batchCommandTest : Test
batchCommandTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
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
          Markup.target "#first-message" result
            |> Markup.expect (element <| hasText "Cool stuff!")
    , test "it processes the second command" <|
      \() ->
          Markup.target "#second-message" result
            |> Markup.expect (element <| hasText "Fun stuff!")
    ]

batchCommandFailureTest : Test
batchCommandFailureTest =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
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
          Expect.equal (ComponentState.failure "It failed!") result
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
    result = Command.send (\() -> Cmd.map AppMsg batchCommand) initialState
  in
    describe "when a batched command is mapped"
    [ test "it maps the first command" <|
      \() ->
          Markup.target "#first-message" result
            |> Markup.expect (element <| hasText "Cool stuff!")
    , test "it maps the second command" <|
      \() ->
          Markup.target "#second-message" result
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
        initialState = Elmer.componentState App.defaultModel App.view App.update
        unknownCommandThunk = \() -> Task.perform App.RenderFirstMessage (Task.succeed "hello")
      in
        Command.send unknownCommandThunk initialState
          |> Expect.equal (ComponentState.failure ( format
            [ message "Elmer encountered a command it does not know how to run" "Task"
            , description "Try sending a stubbed or dummy command instead"
            ]
          ))
  ]
