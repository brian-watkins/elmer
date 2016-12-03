module Elmer.Http exposing
  ( HttpResponseStub
  , HttpResponseResult(..)
  , HttpRequest
  , asHttpRequest
  , fakeHttpSend
  , expectPOST
  )

import Http
import Dict
import Task exposing (Task)
import Json.Encode as Encode
import Elmer
import Elmer.Types exposing (..)
import Elmer.Command as Command
import Expect exposing (Expectation)


type alias HttpRequest a =
  { method: String
  , url: String
  , body: Maybe String
  , responseHandler: (Http.Response String -> Result String a)
  }

type alias HttpResponseStub =
  { url: String
  , method: String
  , response: HttpResponseResult
  }

type HttpResponseResult
  = HttpResponse (Http.Response String)
  | HttpError Http.Error


asHttpRequest : Http.Request a -> HttpRequest a
asHttpRequest request =
  Native.Helpers.asHttpRequest request


fakeHttpSend : HttpResponseStub -> (Result Http.Error a -> msg) -> Http.Request a -> Cmd msg
fakeHttpSend responseStub tagger request =
  let
    httpRequest = asHttpRequest request
  in
    matchRequest responseStub httpRequest
      |> Result.andThen generateResponse
      |> Result.andThen (processResponse httpRequest tagger)
      |> collapseToCommand
      |> toHttpCommand httpRequest


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
    httpCommand = Native.Helpers.toCmd "Elmer_Http" (requestAsJson request)
  in
    Cmd.batch [ httpCommand, command ]

requestAsJson : HttpRequest a -> Encode.Value
requestAsJson request =
  Encode.object
    [ ("method", Encode.string request.method )
    , ("url", Encode.string request.url)
    , ("body", nullOrString request.body)
    ]

nullOrString : Maybe String -> Encode.Value
nullOrString maybeString =
  Maybe.map (\s -> Encode.string s) maybeString
    |> Maybe.withDefault Encode.null


expectPOST : String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectPOST url requestMatcher =
  Elmer.mapToExpectation <|
    \componentState ->
      case hasRequest componentState.httpRequests "POST" url of
        Just request ->
          requestMatcher request
        Nothing ->
          if List.isEmpty componentState.httpRequests then
            Expect.fail ("Expected request for\n\n\tPOST " ++ url ++ "\n\nbut no requests have been made")
          else
            let
              requests = String.join "\n\n\t" (List.map (\r -> r.method ++ " " ++ r.url) componentState.httpRequests)
            in
            Expect.fail ("Expected request for\n\n\tPOST " ++ url ++ "\n\nbut only found these requests\n\n\t" ++ requests)


hasRequest : List HttpRequestData -> String -> String -> Maybe HttpRequestData
hasRequest requests method url =
  List.filter (\r -> r.method == method && r.url == url) requests
    |> List.head

matchRequest : HttpResponseStub -> HttpRequest a -> Result (Cmd msg) HttpResponseStub
matchRequest responseStub httpRequest =
  matchRequestUrl httpRequest responseStub
    |> Result.andThen (matchRequestMethod httpRequest)


matchRequestUrl : HttpRequest a -> HttpResponseStub -> Result (Cmd msg) HttpResponseStub
matchRequestUrl httpRequest responseStub =
  if httpRequest.url == responseStub.url then
    Ok responseStub
  else
    Err (Command.failureCommand ("Received a request for\n\n\t" ++ httpRequest.url ++ "\n\nbut it has not been stubbed. The stubbed request is\n\n\t" ++ responseStub.url))


matchRequestMethod : HttpRequest a -> HttpResponseStub -> Result (Cmd msg) HttpResponseStub
matchRequestMethod httpRequest responseStub =
  if httpRequest.method == responseStub.method then
    Ok responseStub
  else
    Err (Command.failureCommand ("A response has been stubbed for\n\n\t" ++ httpRequest.url ++ "\n\nbut it expects a " ++ responseStub.method ++ " not a " ++ httpRequest.method))


generateResponse : HttpResponseStub -> Result (Cmd msg) (HttpResponseResult)
generateResponse responseStub =
  Ok responseStub.response


processResponse : HttpRequest a -> (Result Http.Error a -> msg) -> HttpResponseResult -> Result (Cmd msg) (Cmd msg)
processResponse httpRequest tagger responseResult =
  handleResponseError responseResult
    |> Result.andThen handleResponseStatus
    |> Result.andThen (handleResponse httpRequest)
    |> Result.mapError (mapResponseError httpRequest tagger)
    |> Result.map (\d -> Task.attempt tagger (Task.succeed d))

mapResponseError : HttpRequest a -> (Result Http.Error a -> msg) -> Http.Error -> Cmd msg
mapResponseError httpRequest tagger error =
  case error of
    Http.BadPayload msg response ->
      Command.failureCommand ("Parsing a stubbed response\n\n\t" ++ httpRequest.method ++ " " ++ httpRequest.url ++
        "\n\n\t" ++ response.body ++ "\n\nfailed with error\n\n\t" ++ msg ++
        "\n\nIf you really want to generate a BadPayload error, consider using\nElmer.Http.Stub.withError to build your stubbed response."
      )
    _ ->
      Task.attempt tagger (Task.fail error)


handleResponseError : HttpResponseResult -> Result Http.Error (Http.Response String)
handleResponseError responseResult =
  case responseResult of
    HttpResponse response ->
      Ok response
    HttpError error ->
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
