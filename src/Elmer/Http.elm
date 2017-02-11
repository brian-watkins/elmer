module Elmer.Http exposing
  ( HttpResponseStub
  , HttpResponseResult(..)
  , HttpRequestFunction
  , HttpRequest
  , asHttpRequest
  , expectPOST
  , expectGET
  , expectDELETE
  , clearRequestHistory
  , serve
  , spy
  )

import Http
import Dict
import Elmer
import Elmer.Types exposing (..)
import Elmer.Command as Command
import Elmer.Platform as Platform exposing (PlatformOverride)
import Elmer.Command.Internal as InternalCommand
import Elmer.Printer exposing (..)
import Expect exposing (Expectation)


type alias HttpRequestFunction a b =
  (Result Http.Error a -> b) -> Http.Request a -> Cmd b

type alias HttpRequest a =
  { method: String
  , url: String
  , headers: List HttpHeader
  , body: Maybe String
  , responseHandler: (Http.Response String -> Result String a)
  }

type alias HttpResponseStub =
  { url: String
  , method: String
  , response: HttpResponseResult
  , deferResponse: Bool
  }

type HttpResponseResult
  = HttpResponse (Http.Response String)
  | HttpError Http.Error


asHttpRequest : Http.Request a -> HttpRequest a
asHttpRequest request =
  Native.Helpers.asHttpRequest request


dummySend : HttpRequestFunction a msg
dummySend _ request =
  let
    httpRequest = asHttpRequest request
  in
    toHttpCommand httpRequest Cmd.none


serve : List HttpResponseStub -> PlatformOverride
serve responseStubs =
  Command.override (\_ -> Http.send) (stubbedSend responseStubs)


spy : PlatformOverride
spy =
  Command.override (\_ -> Http.send) dummySend


stubbedSend : List HttpResponseStub -> HttpRequestFunction a msg
stubbedSend responseStubs tagger request =
  let
    httpRequest = asHttpRequest request
  in
    responseStubsAreValid responseStubs
      |> Result.andThen (matchFirstRequest httpRequest)
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
    requestData =
      { method = request.method
      , url = request.url
      , body = request.body
      , headers = request.headers
      }
    httpCommand = InternalCommand.mapState <| updateComponentState requestData
  in
    Cmd.batch [ httpCommand, command ]


updateComponentState : HttpRequestData -> HtmlComponentState model msg -> HtmlComponentState model msg
updateComponentState requestData componentState =
  { componentState | httpRequests = requestData :: componentState.httpRequests }


clearRequestHistory : ComponentStateResult model msg -> ComponentStateResult model msg
clearRequestHistory =
  Elmer.map (\componentState ->
    if List.isEmpty componentState.httpRequests then
      UpstreamFailure "No HTTP requests to clear"
    else
      CurrentState { componentState | httpRequests = [] }
  )


expectPOST : String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectPOST =
  expectRequest "POST"


expectGET : String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectGET =
  expectRequest "GET"


expectDELETE : String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectDELETE =
  expectRequest "DELETE"


expectRequest : String -> String -> (HttpRequestData -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectRequest method url requestMatcher =
  Elmer.mapToExpectation <|
    \componentState ->
      if String.contains "?" url then
        Expect.fail <| format
          [ message "The expected route contains a query string" url
          , description "Use the hasQueryParam matcher instead"
          ]
      else
        case hasRequest componentState.httpRequests method url of
          Just request ->
            requestMatcher request
          Nothing ->
            if List.isEmpty componentState.httpRequests then
              Expect.fail <| format
                [ message "Expected request for" (method ++ " " ++ url)
                , description "but no requests have been made"
                ]
            else
              let
                requests = String.join "\n\n\t" (List.map (\r -> r.method ++ " " ++ r.url) componentState.httpRequests)
              in
                Expect.fail <| format
                  [ message "Expected request for" (method ++ " " ++ url)
                  , message "but only found these requests" requests
                  ]


hasRequest : List HttpRequestData -> String -> String -> Maybe HttpRequestData
hasRequest requests method url =
  List.filter (\r -> r.method == method && (route r.url) == url) requests
    |> List.head


responseStubsAreValid : List HttpResponseStub -> Result (Cmd msg) (List HttpResponseStub)
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


responseStubIsValid : HttpResponseStub -> Result (Cmd msg) HttpResponseStub
responseStubIsValid responseStub =
  if String.contains "?" responseStub.url then
    Err <| Command.fail <| format
      [ message "Sent a request where a stubbed route contains a query string" responseStub.url
      , description "Stubbed routes may not contain a query string"
      ]
  else
    Ok responseStub


matchFirstRequest : HttpRequest a -> List HttpResponseStub -> Result (Cmd msg) HttpResponseStub
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


printStubs : List HttpResponseStub -> String
printStubs =
  List.foldl (\s msg -> msg ++ (printStub s) ++ "\n") ""


printStub : HttpResponseStub -> String
printStub responseStub =
  responseStub.method ++ " " ++ responseStub.url


matchRequest : HttpRequest a -> HttpResponseStub -> Maybe HttpResponseStub
matchRequest httpRequest responseStub =
  matchRequestUrl httpRequest responseStub
    |> Maybe.andThen (matchRequestMethod httpRequest)


matchRequestUrl : HttpRequest a -> HttpResponseStub -> Maybe HttpResponseStub
matchRequestUrl httpRequest responseStub =
  if (route httpRequest.url) == responseStub.url then
    Just responseStub
  else
    Nothing


matchRequestMethod : HttpRequest a -> HttpResponseStub -> Maybe HttpResponseStub
matchRequestMethod httpRequest responseStub =
  if httpRequest.method == responseStub.method then
    Just responseStub
  else
    Nothing


route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""


generateResponse : HttpResponseStub -> HttpResponseResult
generateResponse responseStub =
  responseStub.response


processResponse : HttpRequest a -> (Result Http.Error a -> msg) -> HttpResponseStub -> Result (Cmd msg) (Cmd msg)
processResponse httpRequest tagger responseStub =
  generateResponse responseStub
    |> handleResponseError
    |> Result.andThen handleResponseStatus
    |> Result.andThen (handleResponse httpRequest)
    |> Result.mapError (mapResponseError httpRequest tagger)
    |> Result.map (generateCommand responseStub tagger)


generateCommand : HttpResponseStub -> (Result Http.Error a -> msg) -> a -> Cmd msg
generateCommand responseStub tagger data =
  let
    command = Command.stub (tagger (Ok data))
  in
    if responseStub.deferResponse then
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
      Command.stub (tagger (Err error))


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
