module Elmer.Runtime
    exposing
        ( performUpdate
        , performCommand
        )

import Elmer.Types exposing (..)
import Dict exposing (Dict)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  HtmlComponentState model msg -> (HtmlComponentState model msg, Cmd msg)

type alias CommandRunner model subMsg msg =
  LeafCommandData subMsg -> (subMsg -> msg) -> CommandResult model msg

type CommandData msg subMsg
    = LeafCommand (LeafCommandData msg)
    | MapCommand (MapCommandData subMsg msg)
    | BatchCommand (List (Cmd msg))
    | NoCommand

type alias MapCommandData subMsg msg =
    { tree : Cmd subMsg
    , tagger : subMsg -> msg
    }

type alias LeafCommandData msg =
    { command : Cmd msg
    , home : String
    }

commandRunners : Dict String (CommandRunner model subMsg msg)
commandRunners =
  Dict.fromList <|
    [ ( "Elmer_Fail", elmerFailureCommandRunner )
    , ( "Elmer_Stub", elmerStubbedCommandRunner )
    , ( "Elmer_Generate", generateCommandRunner )
    , ( "Elmer_MapState", mapStateCommandRunner )
    ]


mapStateCommandRunner : CommandRunner model subMsg msg
mapStateCommandRunner data _ =
  let
    componentStateMapper = Native.Helpers.commandValue data.command
  in
    CommandSuccess (mapComponentState componentStateMapper)

mapComponentState : (HtmlComponentState model msg -> HtmlComponentState model msg) -> HtmlComponentState model msg -> ( HtmlComponentState model msg, Cmd msg )
mapComponentState mapper componentState =
  ( mapper componentState, Cmd.none )


generateCommandRunner : CommandRunner model subMsg msg
generateCommandRunner data _ =
  let
    generator = Native.Helpers.commandValue data.command
  in
    CommandSuccess (generateCommand generator)

generateCommand : (HtmlComponentState model msg -> Cmd msg) -> HtmlComponentState model msg -> ( HtmlComponentState model msg, Cmd msg )
generateCommand generator componentState =
  ( componentState, generator componentState )


elmerFailureCommandRunner : CommandRunner model subMsg msg
elmerFailureCommandRunner data _ =
  let
    message = Native.Helpers.commandValue data.command
  in
    CommandError message


elmerStubbedCommandRunner : CommandRunner model subMsg msg
elmerStubbedCommandRunner data tagger =
  let
    msg = Native.Helpers.commandValue data.command
  in
    CommandSuccess (updateComponentState (tagger msg))


identityRunner : CommandRunner model subMsg msg
identityRunner _ _ =
  CommandSuccess (\componentState -> ( componentState, Cmd.none ))


identityTagger : msg -> msg
identityTagger msg =
  msg


updateComponentState : msg -> HtmlComponentState model msg -> ( HtmlComponentState model msg, Cmd msg )
updateComponentState message componentState =
    let
        ( updatedModel, command ) =
            componentState.update message componentState.model

        updatedState =
            { componentState | model = updatedModel }
    in
        ( updatedState, command )


performUpdate : msg -> HtmlComponentState model msg -> Result String (HtmlComponentState model msg)
performUpdate message componentState =
    let
        ( updatedComponent, command ) =
            updateComponentState message componentState
    in
        performCommand command updatedComponent


commandRunnerForData : String -> CommandRunner model subMsg msg
commandRunnerForData commandName =
  Dict.get commandName commandRunners
    |> Maybe.withDefault identityRunner


performCommand : Cmd msg -> HtmlComponentState model msg -> Result String (HtmlComponentState model msg)
performCommand command componentState =
    let
        commandResults =
            runCommand identityTagger command
    in
      List.foldl reduceCommandResults (Ok componentState) commandResults

reduceCommandResults : CommandResult model msg -> Result String (HtmlComponentState model msg) -> Result String (HtmlComponentState model msg)
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


processCommandEffect : CommandEffect model msg -> HtmlComponentState model msg -> ( HtmlComponentState model msg, Cmd msg )
processCommandEffect commandEffect componentState =
    commandEffect componentState


runCommand : (subMsg -> msg) -> Cmd subMsg -> List (CommandResult model msg)
runCommand tagger command =
    case Native.Helpers.asCommandData command of
        LeafCommand data ->
            let
              runner = commandRunnerForData data.home
              commandResult = runner data tagger
            in
                [ commandResult ]

        MapCommand data ->
          let
            composedTagger = composeFunctions tagger data.tagger
          in
            runCommand (composedTagger) data.tree

        BatchCommand commands ->
          List.concat (List.map (\c -> runCommand tagger c) commands)

        NoCommand ->
          let
            commandResult = CommandSuccess (\componentState -> ( componentState, Cmd.none ))
          in
            [ commandResult ]


composeFunctions : (msg -> parentMsg) -> (subMsg -> msg) -> (subMsg -> parentMsg)
composeFunctions f g =
  f << g
