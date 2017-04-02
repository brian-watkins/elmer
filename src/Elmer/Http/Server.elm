module Elmer.Http.Server exposing
  ( stubbedSend
  , dummySend
  )

import Http
import Dict
import Elmer exposing (Matcher)
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Internal as Internal exposing (..)
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform
import Elmer.Printer exposing (..)
import Expect exposing (Expectation)


stubbedSend : List HttpResponseStub -> HttpRequestFunction a msg
stubbedSend responseStubs tagger request =
  let
    httpRequest = HttpInternal.asHttpRequest request
  in
    unwrapResponseStubs responseStubs
      |> responseStubsAreValid
      |> Result.andThen (matchFirstRequest httpRequest)
      |> Result.andThen (processResponse httpRequest tagger)
      |> collapseToCommand
      |> toHttpCommand httpRequest


dummySend : HttpRequestFunction a msg
dummySend _ request =
  let
    httpRequest = HttpInternal.asHttpRequest request
  in
    toHttpCommand httpRequest Cmd.none




unwrapResponseStubs : List HttpResponseStub -> List HttpStub
unwrapResponseStubs responseStubs =
  List.map (\(HttpResponseStub stub) -> stub) responseStubs

collapseToCommand : Result (Cmd msg) (Cmd msg) -> Cmd msg
collapseToCommand responseResult =
  case responseResult of
    Ok command ->
      command
    Err errorCommand ->
      errorCommand


toHttpCommand : HttpRequest a -> Cmd msg -> Cmd msg
toHttpCommand request command =
  let
    requestData =
      { method = request.method
      , url = request.url
      , body = request.body
      , headers = request.headers
      }
    httpCommand = Platform.mapStateCommand <| updateComponentState requestData
  in
    Cmd.batch [ httpCommand, command ]


updateComponentState : HttpRequestData -> Component model msg -> Component model msg
updateComponentState requestData componentState =
  { componentState | httpRequests = requestData :: componentState.httpRequests }


responseStubsAreValid : List HttpStub -> Result (Cmd msg) (List HttpStub)
responseStubsAreValid responseStubs =
  List.map responseStubIsValid responseStubs
    |> List.foldl (\result totalResult ->
      case result of
        Ok result ->
          case totalResult of
            Ok validStubs ->
              Ok (result :: validStubs)
            Err cmd ->
              Err cmd
        Err cmd ->
          Err cmd
    ) (Ok [])


responseStubIsValid : HttpStub -> Result (Cmd msg) HttpStub
responseStubIsValid responseStub =
  if String.contains "?" responseStub.url then
    Err <| Command.fail <| format
      [ message "Sent a request where a stubbed route contains a query string" responseStub.url
      , description "Stubbed routes may not contain a query string"
      ]
  else
    Ok responseStub


matchFirstRequest : HttpRequest a -> List HttpStub -> Result (Cmd msg) HttpStub
matchFirstRequest httpRequest responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequest) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| Command.fail <| format
        [ message "Received a request for" (printRequest httpRequest)
        , message "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequest a -> String
printRequest request =
  request.method ++ " " ++ request.url


printStubs : List HttpStub -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : HttpStub -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequest a -> HttpStub -> Maybe HttpStub
matchRequest httpRequest stub =
  matchRequestUrl httpRequest stub
    |> Maybe.andThen (matchRequestMethod httpRequest)


matchRequestUrl : HttpRequest a -> HttpStub -> Maybe HttpStub
matchRequestUrl httpRequest stub =
  if (HttpInternal.route httpRequest.url) == stub.url then
    Just stub
  else
    Nothing


matchRequestMethod : HttpRequest a -> HttpStub -> Maybe HttpStub
matchRequestMethod httpRequest stub =
  if httpRequest.method == stub.method then
    Just stub
  else
    Nothing




generateResponse : HttpStub -> HttpResponseResult
generateResponse stub =
  stub.response


processResponse : HttpRequest a -> (Result Http.Error a -> msg) -> HttpStub -> Result (Cmd msg) (Cmd msg)
processResponse httpRequest tagger stub =
  generateResponse stub
    |> handleResponseError
    |> Result.andThen handleResponseStatus
    |> Result.andThen (handleResponse httpRequest)
    |> Result.mapError (mapResponseError httpRequest tagger)
    |> Result.map (generateCommand stub tagger)


generateCommand : HttpStub -> (Result Http.Error a -> msg) -> a -> Cmd msg
generateCommand stub tagger data =
  let
    command = Command.fake (tagger (Ok data))
  in
    if stub.deferResponse then
      Command.defer command
    else
      command


mapResponseError : HttpRequest a -> (Result Http.Error a -> msg) -> Http.Error -> Cmd msg
mapResponseError httpRequest tagger error =
  case error of
    Http.BadPayload msg response ->
      Command.fail <| format
        [ message "Parsing a stubbed response" (httpRequest.method ++ " " ++ httpRequest.url)
        , description ("\t" ++ response.body)
        , message "failed with error" msg
        , description "If you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
        ]
    _ ->
      Command.fake (tagger (Err error))


handleResponseError : HttpResponseResult -> Result Http.Error (Http.Response String)
handleResponseError responseResult =
  case responseResult of
    Response response ->
      Ok response
    Error error ->
      Err error


handleResponseStatus : Http.Response String -> Result Http.Error (Http.Response String)
handleResponseStatus response =
  if response.status.code >= 200 && response.status.code < 300 then
    Ok response
  else
    Err (Http.BadStatus response)


handleResponse : HttpRequest a -> Http.Response String -> Result Http.Error a
handleResponse httpRequest response =
  case httpRequest.responseHandler response of
    Ok data ->
      Ok data
    Err err ->
      Err (Http.BadPayload err response)
