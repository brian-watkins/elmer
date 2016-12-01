module Elmer.Command exposing
  ( failureCommand )


failureCommand : String -> Cmd msg
failureCommand message =
  Native.Helpers.toCmd "Elmer_Failure" message
