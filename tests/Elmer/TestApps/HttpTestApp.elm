module Elmer.TestApps.HttpTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json

type alias Model =
  { dataResult : String
  , query : String
  }

defaultModel : Model
defaultModel =
  { dataResult = ""
  , query = ""
  }

type Msg
  = RequestData
  | WebServiceResponse (Result Http.Error String)

type alias HttpRequestFunction a b =
  (Result Http.Error a -> b) -> Http.Request a -> Cmd b

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
  [ Html.div [ Attr.id "request-data-click", onClick RequestData ]
    [ Html.text "Click for data" ]
  , Html.div [ Attr.id "data-result" ]
    [ Html.text model.dataResult ]
  ]

update : HttpRequestFunction String Msg -> Msg -> Model -> ( Model, Cmd Msg )
update stubbedSend msg model =
  case msg of
    RequestData ->
      ( model, stubbedSend WebServiceResponse (fetchDataRequest model) )
    WebServiceResponse (Ok name) ->
      ( { model | dataResult = name }, Cmd.none )
    WebServiceResponse (Err (Http.BadPayload message response)) ->
      ( { model | dataResult = ("BadPayload Error: " ++ message) }, Cmd.none )
    WebServiceResponse (Err (Http.BadStatus response)) ->
      ( { model | dataResult = ("BadStatus Error: " ++ (toString response.status.code) ++ " " ++ response.status.message) }, Cmd.none )
    WebServiceResponse (Err Http.Timeout) ->
      ( { model | dataResult = "Timeout Error" }, Cmd.none )
    WebServiceResponse (Err _) ->
      ( { model | dataResult = "Error: Some unknown error" }, Cmd.none )

fetchDataRequest : Model -> Http.Request String
fetchDataRequest model =
  Http.request
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = ("http://fun.com/fun.html" ++ model.query)
    , body = Http.emptyBody
    , expect = Http.expectJson webServiceDecoder
    , timeout = Nothing
    , withCredentials = False
    }

webServiceDecoder : Json.Decoder String
webServiceDecoder =
  Json.field "name" Json.string
