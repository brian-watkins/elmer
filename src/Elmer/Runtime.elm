module Elmer.Runtime
    exposing
        ( performUpdate
        , performCommand
        )

import Elmer.Context as Context exposing (Context)
import Elmer.Printer exposing (..)
import Elmer.Platform.Internal as Platform exposing (..)
import Dict exposing (Dict)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  Context model msg -> (Context model msg, Cmd msg)

type alias CommandRunner model subMsg msg =
  Cmd subMsg -> (subMsg -> msg) -> CommandResult model msg


commandRunners : Dict String (CommandRunner model subMsg msg)
commandRunners =
  Dict.fromList <|
    [ ( "Elmer_Fail", elmerFailureCommandRunner )
    , ( "Elmer_Stub", elmerStubbedCommandRunner )
    , ( "Elmer_Generate", generateCommandRunner )
    , ( "Elmer_MapState", mapStateCommandRunner )
    ]


mapStateCommandRunner : CommandRunner model subMsg msg
mapStateCommandRunner command _ =
  let
    testStateMapper = Platform.cmdValue command
  in
    CommandSuccess (mapTestState testStateMapper)

mapTestState : (Context model msg -> Context model msg) -> Context model msg -> ( Context model msg, Cmd msg )
mapTestState mapper testState =
  ( mapper testState, Cmd.none )


generateCommandRunner : CommandRunner model subMsg msg
generateCommandRunner command _ =
  let
    generator = Platform.cmdValue command
  in
    CommandSuccess (generateCommand generator)

generateCommand : (Context model msg -> Cmd msg) -> Context model msg -> ( Context model msg, Cmd msg )
generateCommand generator testState =
  ( testState, generator testState )


elmerFailureCommandRunner : CommandRunner model subMsg msg
elmerFailureCommandRunner command _ =
  let
    message = Platform.cmdValue command
  in
    CommandError message


elmerStubbedCommandRunner : CommandRunner model subMsg msg
elmerStubbedCommandRunner command tagger =
  let
    msg = Platform.cmdValue command
  in
    CommandSuccess (updateTestState (tagger msg))


unknownCommandRunner : String -> CommandRunner model subMsg msg
unknownCommandRunner commandName _ _ =
  CommandError <|
    format <|
      [ message "Elmer encountered a command it does not know how to run" commandName
      , description "Try sending a stubbed or dummy command instead"
      ]


identityTagger : msg -> msg
identityTagger msg =
  msg


updateTestState : msg -> Context model msg -> ( Context model msg, Cmd msg )
updateTestState message testState =
    let
        ( updatedModel, command ) =
            testState.update message testState.model

        updatedState =
            { testState | model = updatedModel }
    in
        ( updatedState, command )


performUpdate : msg -> Context model msg -> Result String (Context model msg)
performUpdate message testState =
    let
        ( updatedComponent, command ) =
            updateTestState message testState
    in
        performCommand command updatedComponent


commandRunnerForData : String -> CommandRunner model subMsg msg
commandRunnerForData commandName =
  Dict.get commandName commandRunners
    |> Maybe.withDefault (unknownCommandRunner commandName)


performCommand : Cmd msg -> Context model msg -> Result String (Context model msg)
performCommand command context =
    let
        commandResults =
            runCommand identityTagger command
    in
      List.foldl reduceCommandResults (Ok context) commandResults

reduceCommandResults : CommandResult model msg -> Result String (Context model msg) -> Result String (Context model msg)
reduceCommandResults commandResult currentResult =
  case currentResult of
    Ok testState ->
      case commandResult of
        CommandSuccess commandEffect ->
          let
            ( updatedTestState, updatedCommand ) = processCommandEffect commandEffect testState
          in
            if updatedCommand == Cmd.none then
              Ok updatedTestState
            else
              performCommand updatedCommand updatedTestState
        CommandError errorMessage ->
          Err errorMessage
    Err errorMessage ->
      Err errorMessage


processCommandEffect : CommandEffect model msg -> Context model msg -> ( Context model msg, Cmd msg )
processCommandEffect commandEffect testState =
    commandEffect testState


runCommand : (subMsg -> msg) -> Cmd subMsg -> List (CommandResult model msg)
runCommand tagger command =
    case Platform.cmdData command of
        Leaf data ->
            let
              runner = commandRunnerForData data.home
              commandResult = runner data.intention tagger
            in
              [ commandResult ]

        Tree data ->
          let
            composedTagger = composeFunctions tagger data.tagger
          in
            runCommand (composedTagger) data.tree

        Batch commands ->
          List.concat (List.map (\c -> runCommand tagger c) commands)

        Unknown ->
          let
            commandResult = CommandSuccess (\testState -> ( testState, Cmd.none ))
          in
            [ commandResult ]


composeFunctions : (msg -> parentMsg) -> (subMsg -> msg) -> (subMsg -> parentMsg)
composeFunctions f g =
  f << g
