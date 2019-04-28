module Elmer.Http.Internal exposing
  ( asHttpRequestHandler
  , route
  , queryString
  , makeHttpRequest
  )

{-| Exposed for Testing

@docs asHttpRequestHandler, route, queryString, makeHttpRequest

-}

import Http
import Json.Decode as Json
import Elmer.Value as Value
import Elmer.Http.Types exposing (..)


{-|
-}
asHttpRequestHandler : Http.Request a -> HttpRequestHandler a
asHttpRequestHandler httpRequest =
  { request = makeHttpRequest httpRequest
  , responseHandler =
      case Value.decode expectDecoder httpRequest of
        Ok handler ->
          handler
        Err err ->
          "Error fetching" ++ Json.errorToString err
            |> Debug.todo
  }


expectDecoder : Json.Decoder a
expectDecoder =
  Value.firstArg <|
    Json.at ["expect", "a"] Value.decoder


{-|
-}
makeHttpRequest : Http.Request a -> HttpRequest
makeHttpRequest request =
  case Value.decode httpRequestDecoder request of
    Ok r -> 
      r
    Err msg ->
      "Unable to decode Http.Request: " ++ Json.errorToString msg
        |> Debug.todo


httpRequestDecoder : Json.Decoder HttpRequest
httpRequestDecoder =
  Value.firstArg <|
    Json.map4 HttpRequest
      (Json.field "method" Json.string)
      (Json.field "url" Json.string)
      (Json.field "headers" (Value.list headerDecoder))
      (Json.field "body" bodyDecoder)


headerDecoder : Json.Decoder HttpHeader
headerDecoder =
  Json.map2 HttpHeader
    (Value.firstArg Json.string)
    (Value.secondArg Json.string)


bodyDecoder : Json.Decoder (Maybe String)
bodyDecoder =
  Value.constructor
    |> Json.andThen (\ctor ->
      if ctor == "StringBody" then
        stringBodyDecoder
          |> Json.map .body
          |> Json.map Just
      else
        Json.succeed Nothing
    )


stringBodyDecoder : Json.Decoder HttpStringBody
stringBodyDecoder =
  Json.map2 HttpStringBody
    (Value.firstArg Json.string)
    (Value.secondArg Json.string)


{-|
-}
route : String -> String
route url =
  String.split "?" url
    |> List.head
    |> Maybe.withDefault ""


{-|
-}
queryString : String -> Maybe String
queryString url =
  String.split "?" url
    |> List.drop 1
    |> List.head
