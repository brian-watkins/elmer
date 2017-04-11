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
    httpRequestHandler = HttpInternal.asHttpRequestHandler request
  in
    unwrapResponseStubs responseStubs
      |> responseStubsAreValid
      |> Result.andThen (matchFirstRequest httpRequestHandler)
      |> Result.andThen (processResponse httpRequestHandler tagger)
      |> collapseToCommand
      |> toHttpCommand httpRequestHandler


dummySend : HttpRequestFunction a msg
dummySend _ request =
  let
    httpRequestHandler = HttpInternal.asHttpRequestHandler request
  in
    toHttpCommand httpRequestHandler Cmd.none




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


toHttpCommand : HttpRequestHandler a -> Cmd msg -> Cmd msg
toHttpCommand requestHandler command =
  let
    httpCommand = Platform.mapStateCommand <| updateComponentState requestHandler.request
  in
    Cmd.batch [ httpCommand, command ]


updateComponentState : HttpRequest -> Component model msg -> Component model msg
updateComponentState request componentState =
  { componentState | httpRequests = request :: componentState.httpRequests }


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


matchFirstRequest : HttpRequestHandler a -> List HttpStub -> Result (Cmd msg) HttpStub
matchFirstRequest httpRequestHandler responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequestHandler) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| Command.fail <| format
        [ message "Received a request for" (printRequest httpRequestHandler)
        , message "but it does not match any of the stubbed requests" (printStubs responseStubs)
        ]


printRequest : HttpRequestHandler a -> String
printRequest requestHandler =
  requestHandler.request.method ++ " " ++ requestHandler.request.url


printStubs : List HttpStub -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : HttpStub -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequestHandler a -> HttpStub -> Maybe HttpStub
matchRequest httpRequestHandler stub =
  matchRequestUrl httpRequestHandler stub
    |> Maybe.andThen (matchRequestMethod httpRequestHandler)


matchRequestUrl : HttpRequestHandler a -> HttpStub -> Maybe HttpStub
matchRequestUrl httpRequestHandler stub =
  if (HttpInternal.route httpRequestHandler.request.url) == stub.url then
    Just stub
  else
    Nothing


matchRequestMethod : HttpRequestHandler a -> HttpStub -> Maybe HttpStub
matchRequestMethod httpRequestHandler stub =
  if httpRequestHandler.request.method == stub.method then
    Just stub
  else
    Nothing




generateResponse : HttpStub -> HttpResponseResult
generateResponse stub =
  stub.response


processResponse : HttpRequestHandler a -> (Result Http.Error a -> msg) -> HttpStub -> Result (Cmd msg) (Cmd msg)
processResponse httpRequestHandler tagger stub =
  generateResponse stub
    |> handleResponseError
    |> Result.andThen handleResponseStatus
    |> Result.andThen (handleResponse httpRequestHandler)
    |> Result.mapError (mapResponseError httpRequestHandler tagger)
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


mapResponseError : HttpRequestHandler a -> (Result Http.Error a -> msg) -> Http.Error -> Cmd msg
mapResponseError httpRequestHandler tagger error =
  case error of
    Http.BadPayload msg response ->
      Command.fail <| format
        [ message "Parsing a stubbed response" (httpRequestHandler.request.method ++ " " ++ httpRequestHandler.request.url)
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


handleResponse : HttpRequestHandler a -> Http.Response String -> Result Http.Error a
handleResponse httpRequestHandler response =
  case httpRequestHandler.responseHandler response of
    Ok data ->
      Ok data
    Err err ->
      Err (Http.BadPayload err response)
