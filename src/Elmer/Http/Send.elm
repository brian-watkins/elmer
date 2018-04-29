module Elmer.Http.Send exposing
  ( stubbedWith
  , spy
  )

import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Http.Server as Server
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Printer exposing (..)
import Http


stubbedWith : List HttpResponseStub -> HttpRequestFunction a msg
stubbedWith responseStubs tagger request =
  case Server.handleRequest responseStubs request of
    Ok response ->
      case response.result of
        Ok data ->
          generateCommand response.stub tagger data
            |> toHttpCommand response.request
        Err error ->
          failCommand response.request tagger error
    Err error ->
      RuntimeCommand.fail error


spy : HttpRequestFunction a msg
spy _ request =
  let
    httpRequestHandler = HttpInternal.asHttpRequestHandler request
  in
    toHttpCommand httpRequestHandler.request Cmd.none


failCommand : HttpRequest -> (Result Http.Error a -> msg) -> Http.Error -> Cmd msg
failCommand httpRequest tagger error =
  case error of
    Http.BadPayload msg response ->
      RuntimeCommand.fail <| format
        [ message "Parsing a stubbed response" (httpRequest.method ++ " " ++ httpRequest.url)
        , description <| "\tWith body: " ++ (printBody response.body)
        , message "failed with error" msg
        , description "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
        ]
    _ ->
      RuntimeCommand.stub (tagger (Err error))


generateCommand : HttpStub -> (Result Http.Error a -> msg) -> a -> Cmd msg
generateCommand stub tagger data =
  let
    command = RuntimeCommand.stub (tagger (Ok data))
  in
    if stub.deferResponse then
      RuntimeCommand.defer command
    else
      command


toHttpCommand : HttpRequest -> Cmd msg -> Cmd msg
toHttpCommand request command =
  let
    httpCommand = RuntimeCommand.mapState Requests <| updateTestState request
  in
    Cmd.batch [ httpCommand, command ]


updateTestState : HttpRequest -> Maybe (List HttpRequest) -> List HttpRequest
updateTestState request maybeRequests =
  Maybe.withDefault [] maybeRequests
    |> (::) request


printBody : String -> String
printBody body =
  "\"" ++ body ++ "\""
