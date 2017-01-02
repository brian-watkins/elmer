module Elmer.Command exposing
  ( failureCommand
  , messageCommand
  )


failureCommand : String -> Cmd msg
failureCommand message =
  Native.Helpers.toCmd "Elmer_Failure" message

messageCommand : msg -> Cmd msg
messageCommand message =
  Native.Helpers.toCmd "Elmer_Message" message
