module Elmer.Runtime
    exposing
        ( performUpdate
        , performCommand
        )

import Elmer.Internal exposing (..)
import Elmer.Printer exposing (..)
import Elmer.Platform as Platform exposing (..)
import Dict exposing (Dict)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  Component model msg -> (Component model msg, Cmd msg)

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
    componentStateMapper = Platform.cmdValue command
  in
    CommandSuccess (mapComponentState componentStateMapper)

mapComponentState : (Component model msg -> Component model msg) -> Component model msg -> ( Component model msg, Cmd msg )
mapComponentState mapper componentState =
  ( mapper componentState, Cmd.none )


generateCommandRunner : CommandRunner model subMsg msg
generateCommandRunner command _ =
  let
    generator = Platform.cmdValue command
  in
    CommandSuccess (generateCommand generator)

generateCommand : (Component model msg -> Cmd msg) -> Component model msg -> ( Component model msg, Cmd msg )
generateCommand generator componentState =
  ( componentState, generator componentState )


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
    CommandSuccess (updateComponentState (tagger msg))


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


updateComponentState : msg -> Component model msg -> ( Component model msg, Cmd msg )
updateComponentState message componentState =
    let
        ( updatedModel, command ) =
            componentState.update message componentState.model

        updatedState =
            { componentState | model = updatedModel }
    in
        ( updatedState, command )


performUpdate : msg -> Component model msg -> Result String (Component model msg)
performUpdate message componentState =
    let
        ( updatedComponent, command ) =
            updateComponentState message componentState
    in
        performCommand command updatedComponent


commandRunnerForData : String -> CommandRunner model subMsg msg
commandRunnerForData commandName =
  Dict.get commandName commandRunners
    |> Maybe.withDefault (unknownCommandRunner commandName)


performCommand : Cmd msg -> Component model msg -> Result String (Component model msg)
performCommand command componentState =
    let
        commandResults =
            runCommand identityTagger command
    in
      List.foldl reduceCommandResults (Ok componentState) commandResults

reduceCommandResults : CommandResult model msg -> Result String (Component model msg) -> Result String (Component model msg)
reduceCommandResults commandResult currentResult =
  case currentResult of
    Ok componentState ->
      case commandResult of
        CommandSuccess commandEffect ->
          let
            ( updatedComponentState, updatedCommand ) = processCommandEffect commandEffect componentState
          in
            if updatedCommand == Cmd.none then
              Ok updatedComponentState
            else
              performCommand updatedCommand updatedComponentState
        CommandError errorMessage ->
          Err errorMessage
    Err errorMessage ->
      Err errorMessage


processCommandEffect : CommandEffect model msg -> Component model msg -> ( Component model msg, Cmd msg )
processCommandEffect commandEffect componentState =
    commandEffect componentState


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
            commandResult = CommandSuccess (\componentState -> ( componentState, Cmd.none ))
          in
            [ commandResult ]


composeFunctions : (msg -> parentMsg) -> (subMsg -> msg) -> (subMsg -> parentMsg)
composeFunctions f g =
  f << g
