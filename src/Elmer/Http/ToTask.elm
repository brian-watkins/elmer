module Elmer.Http.ToTask exposing
  ( stubbedWith
  , spy
  )


import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Server as Server
import Elmer.Runtime.Task as RuntimeTask
import Elmer.Runtime.Command as RuntimeCommand
import Task exposing (Task)
import Http


stubbedWith : List HttpResponseStub -> Http.Request a -> Task Http.Error a
stubbedWith responseStubs request =
  case Server.handleRequest responseStubs request of
    Ok response ->
      httpTask response.request response.result
        |> deferIfNecessary response.stub
    Err error ->
      RuntimeCommand.fail error
        |> RuntimeTask.abortWith


spy : Http.Request a -> Task Http.Error a
spy request =
  HttpInternal.asHttpRequestHandler request
    |> .request
    |> flip andRecordRequestTask RuntimeTask.abandon


httpTask : HttpRequest -> Result Http.Error a -> Task Http.Error a
httpTask request result =
  case result of
    Ok value ->
      Task.succeed value
        |> andRecordRequestTask request
    Err error ->
      Task.fail error
        |> andRecordRequestTask request


andRecordRequestTask : HttpRequest -> Task x a -> Task x a
andRecordRequestTask request =
  updateTestState request
    |> RuntimeTask.mapState Requests


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


deferIfNecessary : HttpStub -> Task Http.Error a -> Task Http.Error a
deferIfNecessary stub httpTask =
  if stub.deferResponse then
    httpTask
      |> RuntimeTask.defer
  else
    httpTask
