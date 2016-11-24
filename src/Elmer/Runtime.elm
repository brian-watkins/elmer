module Elmer.Runtime
    exposing
        ( performUpdate
        , performCommand
        )

import Elmer exposing (..)
import Elmer.Navigation.Runner as ElmerNav
import Json.Decode as Json

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
    { componentState : HtmlComponentState model msg
    , message : Maybe msg
    }

type alias CommandRunner model msg =
    { name : String
    , run : LeafCommandData msg -> HtmlComponentState model msg -> CommandResult model msg
    , update : CommandEffect model msg -> ( HtmlComponentState model msg, Cmd msg )
    }


type CommandData msg a
    = LeafCommand (LeafCommandData msg)
    | MapCommand (MapCommandData msg a)
    | NoCommand


type alias MapCommandData msg a =
    { tree : Cmd msg
    , tagger : msg -> a
    }


type alias LeafCommandData msg =
    { command : Cmd msg
    , home : String
    , json : String
    }


commandRunners : List (CommandRunner model msg)
commandRunners =
    [ taskCommandRunner
    , navigationCommandRunner
    ]


taskCommandRunner : CommandRunner model msg
taskCommandRunner =
    { name = "Task"
    , run =
        \data state ->
          let
            taskResult = Native.Helpers.runTask data.command
          in
            case taskResult of
              Ok msg ->
                CommandSuccess { componentState = state
                , message = Just msg
                }
              Err errorMessage ->
                CommandError errorMessage
    , update = updateWithResult
    }


navigationCommandRunner : CommandRunner model msg
navigationCommandRunner =
    { name = "Navigation"
    , run =
        \data state ->
            let
                url =
                    Result.withDefault "" (Json.decodeString (Json.field "_0" Json.string) data.json)
            in
                CommandSuccess { componentState = { state | location = Just url }
                , message = Nothing
                }
    , update =
        \result ->
            let
                url = Maybe.withDefault "" result.componentState.location
            in
                case result.componentState.locationParser of
                    Just locationParser ->
                      let
                          message = ElmerNav.handleLocationUpdate url locationParser
                      in
                          updateComponentState message result.componentState
                    Nothing ->
                        ( result.componentState, Cmd.none )
    }


identityRunner : CommandRunner model msg
identityRunner =
    { name = ""
    , run =
        \data state ->
            CommandSuccess { componentState = state
            , message = Nothing
            }
    , update =
        \result ->
            ( result.componentState, Cmd.none )
    }


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


commandRunnerForData : String -> CommandRunner model msg
commandRunnerForData commandName =
    Maybe.withDefault identityRunner (List.head (List.filter (\r -> r.name == commandName) commandRunners))


performCommand : Cmd msg -> HtmlComponentState model msg -> Result String (HtmlComponentState model msg)
performCommand command componentState =
    let
        commandResult =
            runCommand command componentState
    in
      case commandResult of
        CommandSuccess commandEffect ->
          let
            ( updatedComponentState, updatedCommand ) =
                processCommandEffect command commandEffect
          in
            if updatedCommand == Cmd.none then
              Ok updatedComponentState
            else
              performCommand updatedCommand updatedComponentState
        CommandError errorMessage ->
          Err errorMessage


processCommandEffect : Cmd msg -> CommandEffect model msg -> ( HtmlComponentState model msg, Cmd msg )
processCommandEffect command commandEffect =
    case Native.Helpers.asCommandData command of
        LeafCommand data ->
            let
                runner =
                    commandRunnerForData data.home
            in
                runner.update commandEffect

        MapCommand data ->
            processCommandEffect data.tree commandEffect

        NoCommand ->
            ( commandEffect.componentState, Cmd.none )


updateWithResult : CommandEffect model msg -> ( HtmlComponentState model msg, Cmd msg )
updateWithResult result =
    case result.message of
        Just message ->
            updateComponentState message result.componentState

        Nothing ->
            ( result.componentState, Cmd.none )


runCommand : Cmd msg -> HtmlComponentState model msg -> CommandResult model msg
runCommand command componentState =
    case Native.Helpers.asCommandData command of
        LeafCommand data ->
            let
                runner =
                    commandRunnerForData data.home
            in
                runner.run data componentState

        MapCommand data ->
            let
                commandResult =
                    runCommand data.tree componentState
            in
                case commandResult of
                  CommandSuccess commandEffect ->
                    case commandEffect.message of
                        Just message ->
                            CommandSuccess { commandEffect | message = Just (data.tagger message) }

                        Nothing ->
                            CommandSuccess commandEffect

                  CommandError errorMessage ->
                    CommandError errorMessage

        NoCommand ->
            CommandSuccess { componentState = componentState
            , message = Nothing
            }
