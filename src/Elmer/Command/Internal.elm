module Elmer.Command.Internal exposing
  ( CommandState(..)
  , testStateWithCommand
  )

import Elmer.TestState as TestState exposing (TestState, TestStateExtension(..))
import Elmer.Context as Context exposing (Context, Update, View(..))
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Runtime as Runtime
import Html exposing (Html)
import Expect


type CommandState
  = Messages


testStateWithCommand : (() -> Cmd msg) -> TestState () msg
testStateWithCommand commandGenerator =
  Context.default (HtmlView emptyView) messageCollectorUpdate
    |> Context.withModel ()
    |> withCommandGenerator commandGenerator
    |> TestState.with


emptyView : model -> Html msg
emptyView model =
  Html.text ""


messageCollectorUpdate : msg -> model -> (model, Cmd msg)
messageCollectorUpdate msg model =
  ( model
  , RuntimeCommand.mapState Messages <|
    \state ->
      Maybe.withDefault [] state
        |> (::) msg
  )


withCommandGenerator : (() -> Cmd msg) -> Context model msg -> Context model msg
withCommandGenerator generator context =
  RuntimeCommand.mapState MapBeforeExpectationExtension (\state ->
    Maybe.withDefault [] state
      |> (::) (beforeExpectationExtension generator)
  )
    |> Context.updateStateFor context


beforeExpectationExtension : (() -> Cmd msg) -> Context model msg -> TestState model msg
beforeExpectationExtension commandGenerator context =
  Runtime.performCommand (commandGenerator ()) context
    |> TestState.fromRuntimeResult
