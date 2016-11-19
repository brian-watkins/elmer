module Elmer.Runtime exposing
  ( performUpdate
  , performCommand
  )

import Elmer exposing (..)
import Elmer.Navigation.Runner as ElmerNav

import Json.Decode as Json exposing ((:=))



type alias CommandResult navData model msg =
  { componentState : HtmlComponentState navData model msg
  , message : Maybe msg
  }

type alias CommandRunner navData model msg =
  { name: String
  , run: LeafCommandData msg -> HtmlComponentState navData model msg -> CommandResult navData model msg
  , update: CommandResult navData model msg -> ( HtmlComponentState navData model msg, Cmd msg )
  }

type CommandData msg a
  = LeafCommand (LeafCommandData msg)
  | MapCommand (MapCommandData msg a)
  | NoCommand

type alias MapCommandData msg a =
  { tree: Cmd msg
  , tagger: (msg -> a)
  }

type alias LeafCommandData msg =
  { command: Cmd msg
  , home: String
  , json: String
  }


commandRunners : List (CommandRunner navData model msg)
commandRunners =
  [ taskCommandRunner
  , navigationCommandRunner
  ]

taskCommandRunner : CommandRunner navData model msg
taskCommandRunner =
  { name = "Task"
  , run = \data state ->
      { componentState = state
      , message = Just (Native.Helpers.runTask data.command)
      }
  , update = updateWithResult
  }

navigationCommandRunner : CommandRunner navData model msg
navigationCommandRunner =
  { name = "Navigation"
  , run = \data state ->
      let
        url = Result.withDefault "" (Json.decodeString ("_0" := Json.string) data.json)
        updatedComponentState = { state | location = Just url }
      in
        { componentState = updatedComponentState
        , message = Nothing
        }
  , update = \result ->
      let
        componentState = result.componentState
        url = Maybe.withDefault "" componentState.location
      in
        case componentState.locationParser of
          Just locationParser ->
            case componentState.urlUpdate of
              Just urlUpdate ->
                let
                  (updatedModel, updatedCmd) = ElmerNav.updateLocation url locationParser urlUpdate componentState.model
                  updatedComponentState = { componentState | model = updatedModel }
                in
                  ( updatedComponentState, updatedCmd )
              Nothing ->
                ( componentState, Cmd.none )
          Nothing ->
            ( componentState, Cmd.none )
  }

identityRunner : CommandRunner navData model msg
identityRunner =
  { name = ""
  , run = \data state ->
      { componentState = state
      , message = Nothing
      }
  , update = \result ->
      ( result.componentState, Cmd.none )
  }

updateComponentState : msg -> HtmlComponentState navData model msg -> (HtmlComponentState navData model msg, Cmd msg)
updateComponentState message componentState =
  let
    (updatedModel, command) = componentState.update message componentState.model
    updatedState = { componentState | model = updatedModel }
  in
    (updatedState, command)


performUpdate : msg -> HtmlComponentState navData model msg -> HtmlComponentState navData model msg
performUpdate message componentState =
  let
    (updatedComponent, command) = updateComponentState message componentState
  in
    performCommand command updatedComponent


commandRunnerForData : String -> CommandRunner navData model msg
commandRunnerForData commandName =
  Maybe.withDefault identityRunner (List.head (List.filter (\r -> r.name == commandName) commandRunners))
  

performCommand : Cmd msg -> HtmlComponentState navData model msg -> HtmlComponentState navData model msg
performCommand command componentState =
  let
    commandResult = runCommand command componentState
    (updatedComponentState, updatedCommand) = processCommandResult command commandResult
  in
    if updatedCommand == Cmd.none then
      updatedComponentState
    else
      performCommand updatedCommand updatedComponentState


processCommandResult : Cmd msg -> CommandResult navData model msg -> (HtmlComponentState navData model msg, Cmd msg)
processCommandResult command commandResult =
  case Native.Helpers.asCommandData command of
    LeafCommand data ->
      let
        runner = commandRunnerForData data.home
      in
        runner.update commandResult
    MapCommand data ->
      processCommandResult data.tree commandResult
    NoCommand ->
      (commandResult.componentState, Cmd.none)


updateWithResult : CommandResult navData model msg -> (HtmlComponentState navData model msg, Cmd msg)
updateWithResult result =
  case result.message of
    Just message ->
      updateComponentState message result.componentState
    Nothing ->
      ( result.componentState, Cmd.none )


runCommand : Cmd msg -> HtmlComponentState navData model msg -> CommandResult navData model msg
runCommand command componentState =
  case Native.Helpers.asCommandData command of
    LeafCommand data ->
      let
        runner = commandRunnerForData data.home
      in
        runner.run data componentState
    MapCommand data ->
      let
        commandResult = runCommand data.tree componentState
      in
        case commandResult.message of
          Just message ->
            { commandResult | message = Just (data.tagger message) }
          Nothing ->
            commandResult
    NoCommand ->
      { componentState = componentState
      , message = Nothing
      }


nullLeaf : LeafCommandData msg
nullLeaf =
  { command = Cmd.none
  , home = ""
  , json = ""
  }
