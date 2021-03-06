module Elmer.Runtime exposing
  ( performUpdate
  , performCommand
  )

{-| Exposed for Testing

@docs performUpdate, performCommand

-}

import Elmer.Context as Context exposing (Context)
import Elmer.Runtime.Types exposing (..)
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Runtime.Intention as Intention exposing (Intention(..))
import Elmer.Errors as Errors



{-|
-}
performUpdate : msg -> Context model msg -> RuntimeResult model msg
performUpdate message context =
    case Context.update message context of
      Just ( updatedContext, command ) ->
        performCommand command updatedContext
      Nothing ->
        Err <| Errors.print Errors.noModel


{-|
-}
performCommand : Cmd msg -> Context model msg -> RuntimeResult model msg
performCommand command context =
    let
        commandResults =
            runCommand identity command
    in
      List.foldl reduceCommandResults (Ok context) commandResults


reduceCommandResults : CommandResult model msg -> RuntimeResult model msg -> RuntimeResult model msg
reduceCommandResults commandResult currentResult =
  case currentResult of
    Ok context ->
      case commandResult of
        CommandSuccess commandEffect ->
          let
            ( updatedContext, updatedCommand ) = processCommandEffect commandEffect context
          in
            if updatedCommand == Cmd.none then
              Ok updatedContext
            else
              performCommand updatedCommand updatedContext
        CommandError errorMessage ->
          Err errorMessage
    Err errorMessage ->
      Err errorMessage


processCommandEffect : CommandEffect model msg -> Context model msg -> ( Context model msg, Cmd msg )
processCommandEffect commandEffect context =
    commandEffect context


runCommand : (subMsg -> msg) -> Cmd subMsg -> List (CommandResult model msg)
runCommand tagger command =
    case Intention.cmdData command of
        Leaf data ->
            let
              runner = RuntimeCommand.commandRunner data.home
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
