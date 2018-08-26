module Elmer.TestApps.NavigationTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (Document, UrlRequest(..))
import Url exposing (Url)

import Elmer.Navigation


type alias Model =
  { error: String
  , navigationKey : Navigation.Key
  }

type Msg
  = DoPushUrl
  | DoReplaceUrl
  | DoLoadUrl
  | DoPushBadUrl
  | DoReplaceBadUrl
  | OnUrlRequest UrlRequest
  | OnUrlChange Url


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
  ( { error = ""
    , navigationKey = key
    }
  , Cmd.none
  )

defaultModel : Model
defaultModel =
  { error = ""
  , navigationKey = Elmer.Navigation.fakeKey
  }

view : Model -> Document Msg
view model =
  { title = "Navigation Test App"
  , body = [ pageView model ]
  }

pageView : Model -> Html Msg
pageView model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "pushUrlButton", onClick DoPushUrl ] [ Html.text "Click to navigate!" ]
    , Html.div [ Attr.id "replaceUrlButton", onClick DoReplaceUrl ] [ Html.text "Click to also navigate!" ]
    , Html.div [ Attr.id "loadUrlButton", onClick DoLoadUrl ] [ Html.text "Click to load a new url!" ]
    , Html.div [ Attr.id "pushBadUrl", onClick DoPushBadUrl ] [ Html.text "Click for bad url!" ]
    , Html.div [ Attr.id "replaceBadUrl", onClick DoReplaceBadUrl ] [ Html.text "Click for bad url!" ]
    , Html.div [ Attr.class "error" ] [ Html.text model.error ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoPushUrl ->
      ( model, Navigation.pushUrl model.navigationKey "http://fun.com/fun.html" )
    DoReplaceUrl ->
      ( model, Navigation.replaceUrl model.navigationKey "http://fun.com/awesome.html" )
    DoLoadUrl ->
      ( model, Navigation.load "http://somewhere.com/anotherPlace.html" )
    DoPushBadUrl ->
      ( model, Navigation.pushUrl model.navigationKey "kdshjfkdsjhfksd" )
    DoReplaceBadUrl ->
      ( model, Navigation.replaceUrl model.navigationKey "kdshjfkdsjhfksd" )
    OnUrlRequest urlRequest ->
      case urlRequest of
        Internal url ->
          ( model, Navigation.pushUrl model.navigationKey <| Url.toString url )
        External location ->
          ( model, Navigation.load location )
    OnUrlChange url ->
      if url.path == "/api/view" then
        ( { model | error = "No error" }, Cmd.none )
      else
        ( { model | error = "Unknown path: " ++ url.path }, Cmd.none )

