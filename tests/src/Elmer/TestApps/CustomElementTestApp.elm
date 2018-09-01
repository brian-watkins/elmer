module Elmer.TestApps.CustomElementTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Markdown

type Msg
  = DoSomething
  | HandleBubbledEvent

type alias Model =
  { name : String
  , markdown : String
  }

defaultModel : String -> Model
defaultModel markdown =
  { name = "Dude"
  , markdown = markdown
  }

view : Model -> Html Msg
view model =
  Html.div [ Events.onClick HandleBubbledEvent ]
  [ Markdown.toHtml 
    [ Attr.id "markdown-content"
    , Attr.attribute "data-attr" "funStuff" 
    , Attr.style "position" "absolute"
    , Events.onClick DoSomething
    ] 
    model.markdown
  ]

bubbleView : Model -> Html Msg
bubbleView model =
  Html.div [ Events.onClick HandleBubbledEvent ]
  [ Markdown.toHtml 
    [ Attr.id "markdown-content"
    , Attr.attribute "data-attr" model.name 
    , Attr.style "position" "absolute"
    ] 
    model.markdown
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoSomething ->
      ( model, Cmd.none )
    HandleBubbledEvent ->
      ( { model | name = "Bubbled" }, Cmd.none )
