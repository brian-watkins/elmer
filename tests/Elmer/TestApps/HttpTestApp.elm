module Elmer.TestApps.HttpTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task

type alias Model =
  { dataResult : String
  , otherDataResult : String
  , query : String
  }

defaultModel : Model
defaultModel =
  { dataResult = ""
  , otherDataResult = ""
  , query = ""
  }

type Msg
  = RequestData
  | RequestDataWithTask
  | WebServiceResponse (Result Http.Error String)
  | RequestOtherData
  | RequestOtherDataWithTask
  | OtherWebServiceResponse (Result Http.Error String)

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
  [ Html.div [ Attr.id "request-data-click", onClick RequestData ]
    [ Html.text "Click for data" ]
  , Html.div [ Attr.id "request-data-with-task-click", onClick RequestDataWithTask ]
    [ Html.text "Click for data with task" ]
  , Html.div [ Attr.id "data-result" ]
    [ Html.text model.dataResult ]
  , Html.div [ Attr.id "request-other-data-click", onClick RequestOtherData ]
    [ Html.text "Click for other data" ]
  , Html.div [ Attr.id "request-other-data-with-task-click", onClick RequestOtherDataWithTask ]
    [ Html.text "Click for other data with tasj" ]
  , Html.div [ Attr.id "other-data-result" ]
    [ Html.text model.otherDataResult ]
  ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    RequestData ->
      ( model, Http.send WebServiceResponse (fetchDataRequest model) )
    RequestDataWithTask ->
      ( model
      , Task.sequence
          [ fetchDataRequest model
              |> Http.toTask
          , otherFetchDataRequest model
              |> Http.toTask
          ]
          |> Task.map (\dataList ->
            "Data from Http Task: " ++ (String.join " " dataList)
          )
          |> Task.attempt WebServiceResponse
      )
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
    RequestOtherData ->
      ( model, Http.send OtherWebServiceResponse (otherFetchDataRequest model) )
    RequestOtherDataWithTask ->
      ( model
      , fetchDataRequest model
          |> Http.toTask
          |> Task.andThen (\name -> Http.toTask <| anotherRequest name )
          |> Task.map (\data ->
            "Data from Task: " ++ data
          )
          |> Task.attempt OtherWebServiceResponse
      )
    OtherWebServiceResponse (Ok message) ->
      ( { model | otherDataResult = message }, Cmd.none )
    OtherWebServiceResponse (Err _) ->
      ( { model | otherDataResult = "Error!" }, Cmd.none )

sendRequest : Model -> Cmd Msg
sendRequest model =
  Http.send WebServiceResponse <| fetchDataRequest model

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

otherFetchDataRequest : Model -> Http.Request String
otherFetchDataRequest model =
  Http.request
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = "http://fun.com/super.html"
    , body = Http.emptyBody
    , expect = Http.expectJson otherWebServiceDecoder
    , timeout = Nothing
    , withCredentials = False
    }

anotherRequest : String -> Http.Request String
anotherRequest name =
  Http.request
    { method = "GET"
    , headers = [ Http.header "x-fun" "fun", Http.header "x-awesome" "awesome" ]
    , url = "http://fun.com/super.html?name=" ++ name
    , body = Http.emptyBody
    , expect = Http.expectJson otherWebServiceDecoder
    , timeout = Nothing
    , withCredentials = False
    }

webServiceDecoder : Json.Decoder String
webServiceDecoder =
  Json.field "name" Json.string

otherWebServiceDecoder : Json.Decoder String
otherWebServiceDecoder =
  Json.field "message" Json.string
