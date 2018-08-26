module Elmer.TestApps.DemoTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest(..), Document)
import Url exposing (Url)
import Http

type alias Model =
  { clicks : Int
  , route : Route
  , requestComponentModel : RequestComponentModel
  , navigationKey : Navigation.Key
  }

type Msg
  = DoClick
  | RequestComponentMsgWrapper RequestComponentMsg
  | OnUrlRequest UrlRequest
  | OnUrlChange Url

type Route
  = ClickRoute
  | TextRoute
  | RequestRoute
  | UnknownRoute


init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url key =
  ( { clicks = 0
    , route = UnknownRoute
    , requestComponentModel = requestComponentDefaultModel
    , navigationKey = key
    }
  , Navigation.replaceUrl key (Url.toString url)
  )

document : Model -> Document Msg
document model =
  { title = "Demo App"
  , body = [ view model ]
  }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ] [ viewForRoute model ]

viewForRoute : Model -> Html Msg
viewForRoute model =
  case model.route of
    ClickRoute ->
      clickView model
    TextRoute ->
      textView model
    RequestRoute ->
      requestView model
    UnknownRoute ->
      Html.text "Unknown route!"

clickView : Model -> Html Msg
clickView model =
  Html.div [ Attr.id "clickView" ]
  [ Html.div [ Attr.id "clickCount" ] [ Html.text <| (String.fromInt model.clicks) ++ " clicks!" ]
  , Html.div [ Attr.class "button", onClick DoClick ] [ Html.text "Click me" ]
  ]

textView : Model -> Html Msg
textView model =
  Html.div [ Attr.id "listView" ]
  [ Html.ul []
    [ Html.li [] [ Html.text "Fun Item 1" ]
    , Html.li [] [ Html.text "Fun Item 2" ]
    , Html.li [] [ Html.text "Fun Item 3" ]
    ]
  ]

requestView : Model -> Html Msg
requestView model =
  Html.map RequestComponentMsgWrapper (requestComponentView model.requestComponentModel)

update :  Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    RequestComponentMsgWrapper requestMsg ->
      let
        ( updatedRequestModel, updatedRequestMsg ) = requestComponentUpdate requestMsg model.requestComponentModel
      in
        ( { model | requestComponentModel = updatedRequestModel }
        , Cmd.map RequestComponentMsgWrapper updatedRequestMsg
        )
    OnUrlRequest urlRequest ->
      case urlRequest of
        Internal url ->
          ( model, Navigation.pushUrl model.navigationKey (Url.toString url) )
        External _ ->
          ( model, Cmd.none )
    OnUrlChange url ->
      if url.path == "/click" then
        ( { model | route = ClickRoute }, Cmd.none )
      else if url.path == "/text" then
        ( { model | route = TextRoute }, Cmd.none )
      else if url.path == "/request" then
        ( { model | route = RequestRoute }, Cmd.none )
      else
        ( { model | route = UnknownRoute }, Cmd.none )


-- Request component

type alias RequestComponentModel =
  { responseData: Maybe String
  , responseError: Maybe String
  }

requestComponentDefaultModel : RequestComponentModel
requestComponentDefaultModel =
  { responseData = Nothing
  , responseError = Nothing
  }

type RequestComponentMsg
  = DoRequest
  | HandleResponse (Result Http.Error String)

requestComponentView : RequestComponentModel -> Html RequestComponentMsg
requestComponentView model =
  Html.div []
  [ Html.div [ Attr.id "requestButton", onClick DoRequest ] [ Html.text "Make Request" ]
  , Html.div [ Attr.id "requestOutput" ] [ Html.text ("Response: " ++ (responseOutput model) ) ]
  , Html.div [ Attr.id "requestError" ] [ Html.text ("Got request error: " ++ (requestError model) ) ]
  ]

responseOutput : RequestComponentModel -> String
responseOutput model =
  Maybe.withDefault "No response!" model.responseData

requestError : RequestComponentModel -> String
requestError model =
  Maybe.withDefault "No error!" model.responseError

type alias HttpRequestFunction a b =
  (Result Http.Error a -> b) -> Http.Request a -> Cmd b

requestComponentUpdate : RequestComponentMsg -> RequestComponentModel -> ( RequestComponentModel, Cmd RequestComponentMsg )
requestComponentUpdate msg model =
  case msg of
    DoRequest ->
      ( model, Http.send HandleResponse request )
    HandleResponse result ->
      case result of
        Ok message ->
          ( { model | responseData = Just message, responseError = Nothing }, Cmd.none )
        Err (Http.BadStatus response) ->
          ( { model | responseData = Just "Error!", responseError = Just (errorStatus response) }, Cmd.none )
        Err _ ->
          ( { model | responseData = Just "Error!", responseError = Just "unknown" }, Cmd.none )

errorStatus : Http.Response String -> String
errorStatus response =
  "Bad Status: " ++ (String.fromInt response.status.code) ++ " " ++ response.status.message

request : Http.Request String
request =
  Http.getString "/api/request"
