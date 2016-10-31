module Elmer.TestApp exposing (..)

import Html exposing (Html, div, text, input, Attribute, li, ul, p, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Task exposing (Task)
import Json.Decode as Json exposing (..)
import Navigation

type alias Model =
  { name: String
  , activity: String
  , clicks: Int
  , numberFromTask: Int
  , numberTaskError: String
  , numberTaskGenerator: Task String Int
  , lastLetter : Int
  }

defaultModel : Model
defaultModel =
  { name = "Brian"
  , activity = "reading"
  , clicks = 0
  , numberFromTask = -1
  , numberTaskError = "No error"
  , numberTaskGenerator = (makeNumberTaskThatSucceeds True)
  , lastLetter = -1
  }

onlyText : Html Msg
onlyText =
  text "Only Text"

type Msg =
  HandleClick |
  ClickForNumber |
  TaskNumber Int |
  HandleNumberTaskError String |
  HandleInput String |
  HandleKeyUp Int |
  HandleOtherInput String |
  NavigationClick |
  ModifyNavigationClick

view : Model -> Html Msg
view model =
    div [ id "root", class "content" ]
      [ div [ id "userNameLabel", class "label" ] []
      , div
        [ classList [ ("awesome", True), ("super", True), ("root", True) ] ]
        []
      , div [ class "withText" ]
        [ text "Some Fun Text"
        , div [ class "anotherWithText", attribute "data-special-node" "specialStuff" ]
          [ p [] [ text "my text" ]
          , p [ class "special", attribute "data-special-node" "differentSpecialStuff" ] [ text "special!" ]
          , p [ class "specialer", attribute "data-special-node" "moreSpecialStuff" ] [ text "more special!" ]
          , a [ id "fun-link", href "http://fun.com/fun.html" ] [ text "link to fun!" ]
          ]
        , text "Some more text"
        ]
      , input [ class "nameField", onInput HandleInput, onKeyUp HandleKeyUp ] []
      , div [ class "button", onClick HandleClick ] [ text "Click Me" ]
      , div [ id "clickCount" ] [ text ((toString model.clicks) ++ " clicks!") ]
      , div [ id "numberButton", onClick ClickForNumber ] [ text "Get a number!" ]
      , div [ id "numberOutput" ] [ text ("Clicked and got number: " ++ ((toString model.numberFromTask))) ]
      , div [ id "numberOutputError" ] [ text ("Got error requesting number: " ++ model.numberTaskError)]
      , div [ id "navigationClick", onClick NavigationClick ] [ text "Click to change the URL" ]
      , div [ id "modifyNavigationClick", onClick ModifyNavigationClick ] [ text "Click to modify the URL" ]
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    HandleInput inputString ->
      ( { model | name = inputString }, Cmd.none )
    HandleOtherInput inputString ->
      ( { model | activity = inputString }, Cmd.none )
    ClickForNumber ->
      ( model, Task.perform HandleNumberTaskError TaskNumber model.numberTaskGenerator )
    TaskNumber number ->
      ( { model | numberFromTask = number }, Cmd.none )
    HandleNumberTaskError message ->
      ( { model | numberTaskError = message }, Cmd.none )
    HandleKeyUp key ->
      ( { model | lastLetter = key }, Cmd.none )
    NavigationClick ->
      ( model, Navigation.newUrl "http://fun.com/fun.html" )
    ModifyNavigationClick ->
      ( model, Navigation.modifyUrl "http://fun.com/evenMoreFun.html" )

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)

makeNumberTaskThatSucceeds : Bool -> Task String Int
makeNumberTaskThatSucceeds shouldSucceed =
  if shouldSucceed then
    Task.succeed 3
  else
    Task.fail "Bad things happened!"

eventView : Model -> Html Msg
eventView model =
  input [ id "nameLabel", classList [ ("nameField", True), ("awesome", True) ], onInput HandleInput ] [ text "Click Me" ]
