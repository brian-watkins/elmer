module Elmer.Http.Server exposing
  ( HttpServerResult
  , handleRequest
  )


import Http exposing (Error(..))
import Elmer.Http.Internal as HttpInternal exposing (..)
import Elmer.Printer exposing (..)


type alias HttpServerResult a =
  { request : HttpRequest
  , stub: HttpStub
  , result: Result Http.Error a
  }


handleRequest : List HttpResponseStub -> Http.Request a -> Result String (HttpServerResult a)
handleRequest responseStubs request =
  let
    httpRequestHandler = HttpInternal.asHttpRequestHandler request
  in
    unwrapResponseStubs responseStubs
      |> matchFirstRequest httpRequestHandler
      |> Result.map (\stub ->
        { request = httpRequestHandler.request
        , stub = stub
        , result = processResponse httpRequestHandler stub
        }
      )


unwrapResponseStubs : List HttpResponseStub -> List HttpStub
unwrapResponseStubs responseStubs =
  List.map (\(HttpResponseStub stub) -> stub) responseStubs


matchFirstRequest : HttpRequestHandler a -> List HttpStub -> Result String HttpStub
matchFirstRequest httpRequestHandler responseStubs =
  case List.head <| List.filterMap (matchRequest httpRequestHandler) responseStubs of
    Just matchingResponseStub ->
      Ok matchingResponseStub
    Nothing ->
      Err <| format
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


generateResult : HttpRequest -> HttpStub -> HttpResult
generateResult request stub =
  stub.resultBuilder request


processResponse : HttpRequestHandler a -> HttpStub -> Result Http.Error a
processResponse httpRequestHandler stub =
  generateResult httpRequestHandler.request stub
    |> handleResponseError
    |> Result.andThen handleResponseStatus
    |> Result.andThen (handleResponse httpRequestHandler)


handleResponseError : HttpResult -> Result Http.Error (Http.Response String)
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
