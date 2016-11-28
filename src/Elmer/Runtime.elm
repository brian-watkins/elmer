module Elmer.Runtime
    exposing
        ( performUpdate
        , performCommand
        )

import Elmer.Types exposing (..)
import Elmer.Navigation.Runner as ElmerNav
import Json.Decode as Json
import Dict exposing (Dict)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  HtmlComponentState model msg -> (HtmlComponentState model msg, Cmd msg)

type alias CommandRunner model subMsg msg =
    { name : String
    , run : LeafCommandData subMsg -> (subMsg -> msg) -> CommandResult model msg
    }

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
    , json : String
    }


commandRunners : List (CommandRunner model subMsg msg)
commandRunners =
    [ taskCommandRunner
    , navigationCommandRunner
    ]


taskCommandRunner : CommandRunner model subMsg msg
taskCommandRunner =
    { name = "Task"
    , run =
        \data tagger ->
          let
            taskResult = Native.Helpers.runTask data.command
          in
            case taskResult of
              Ok msg ->
                CommandSuccess (updateComponentState (tagger msg))
              Err errorMessage ->
                CommandError errorMessage
    }


navigationCommandRunner : CommandRunner model subMsg msg
navigationCommandRunner =
    { name = "Navigation"
    , run =
        \data _ ->
            let
                url =
                    Result.withDefault "" (Json.decodeString (Json.field "_0" Json.string) data.json)
            in
                CommandSuccess (updateComponentStateWithLocation url)
    }

updateComponentStateWithLocation : String -> HtmlComponentState model msg -> ( HtmlComponentState model msg, Cmd msg )
updateComponentStateWithLocation url componentState =
  let
      updatedComponentState = { componentState | location = Just url }
  in
      case updatedComponentState.locationParser of
          Just locationParser ->
            let
                message = ElmerNav.handleLocationUpdate url locationParser
            in
                updateComponentState message updatedComponentState
          Nothing ->
              ( updatedComponentState, Cmd.none )


identityRunner : CommandRunner model subMsg msg
identityRunner =
    { name = ""
    , run =
        \_ _ ->
            CommandSuccess (\componentState -> ( componentState, Cmd.none ))
    }

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
    Maybe.withDefault identityRunner (List.head (List.filter (\r -> r.name == commandName) commandRunners))


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
                runner =
                    commandRunnerForData data.home
                commandResult = runner.run data tagger
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
