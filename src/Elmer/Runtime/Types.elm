module Elmer.Runtime.Types exposing
  ( CommandResult(..)
  , CommandEffect
  , CommandRunner
  )

import Elmer.Context exposing (Context)

type CommandResult model msg
  = CommandSuccess (CommandEffect model msg)
  | CommandError String

type alias CommandEffect model msg =
  Context model msg -> (Context model msg, Cmd msg)

type alias CommandRunner model subMsg msg =
  Cmd subMsg -> (subMsg -> msg) -> CommandResult model msg
