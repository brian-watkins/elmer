module Elmer.Runtime.Types exposing
  ( CommandResult(..)
  , CommandEffect
  , CommandRunner
  , RuntimeResult
  )

import Elmer.Context exposing (Context)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  Context model msg -> (Context model msg, Cmd msg)

type alias CommandRunner model subMsg msg =
  Cmd subMsg -> (subMsg -> msg) -> CommandResult model msg

type alias RuntimeResult model msg =
  Result String (Context model msg)
