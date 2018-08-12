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
  -- let
  --   d = Elm.Kernel.Value.print "http request" httpRequest
  -- in
  { request = makeHttpRequest httpRequest
  , responseHandler =
      case Value.mapArg (Value.decode expectDecoder) httpRequest of
        Ok handler ->
          handler
        Err err ->
          Debug.todo <| "Error fetching" ++ err
  }

expectDecoder : Json.Decoder Json.Value
expectDecoder =
  Json.at ["expect", "a"] Value.decoder


{-|
-}
makeHttpRequest : Http.Request a -> HttpRequest
makeHttpRequest =
  Value.mapArg <|
    \r ->
      { method = Value.field "method" r
      , url = Value.field "url" r
      , headers = List.map makeHttpHeader <| Value.field "headers" r
      , body = makeBody r
      }


makeBody : a -> Maybe String
makeBody request =
  let
    body = Value.field "body" request
  in
    if Value.constructor body == "StringBody" then
      Value.mapArg2 HttpStringBody body
        |> .body
        |> Just
    else
      Nothing


makeHttpHeader : Http.Header -> HttpHeader
makeHttpHeader header =
  Value.mapArg2 HttpHeader header

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
