module Elmer.TestApps.InputTestApp exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events exposing (onInput, on, keyCode, onCheck, onSubmit, onClick)
import Json.Decode as Json

type alias Model =
  { name : String
  , lastLetter : Int
  , isChecked : Bool
  , isSubmitted : Bool
  }

type Msg
  = DoInput String
  | DoKeyUp Int
  | HandleCheck Bool
  | HandleSubmit
  | HandleClick

defaultModel : Model
defaultModel =
  { name = ""
  , lastLetter = -1
  , isChecked = False
  , isSubmitted = False
  }

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root", onClick HandleClick ]
    [ Html.form [ Attr.id "my-form", onSubmit HandleSubmit ]
      [ Html.div []
        [ Html.text "First name"
        , Html.input [ Attr.type_ "text", Attr.name "first-name", onInput DoInput, onKeyUp DoKeyUp ] []
        ]
      , Html.div []
        [ Html.text "Are you cool?"
        , Html.input
          [ Attr.type_ "checkbox", Attr.name "is-cool", Attr.checked model.isChecked, onCheck HandleCheck ]
          []
        ]
      , Html.div []
        [ Html.text "I do nothing"
        , Html.input [ Attr.type_ "checkbox", Attr.name "no-op" ] []
        ]
      , Html.input [ Attr.type_ "submit", Attr.name "Submit My Form" ] []
      , Html.button [ Attr.type_ "submit" ] [ Html.text "Submit My Form" ]
      , Html.button [ Attr.id "default-type-button" ] [ Html.text "Submit My Form By Default" ]
      , Html.button [ Attr.type_ "button" ] [ Html.text "I will not submit the form" ]
      ]
    ]

submitWithoutFormView : Model -> Html Msg
submitWithoutFormView model =
  Html.div [ Attr.id "root" ]
  [ Html.input [ Attr.type_ "submit", Attr.name "Submit no form" ] []
  , Html.button [ Attr.type_ "submit" ] [ Html.text "Submit no form" ]
  , Html.button [ Attr.id "default-type-button" ] [ Html.text "Submit no form By Default" ]
  ]

submitOutsideFormView : Model -> Html Msg
submitOutsideFormView model =
  Html.div [ Attr.id "root" ]
    [ Html.form [ Attr.id "my-form", onSubmit HandleSubmit ] []
    , Html.input [ Attr.type_ "submit", Attr.form "my-form", Attr.name "Submit My Form" ] []
    , Html.button [ Attr.type_ "submit", Attr.form "my-form" ] [ Html.text "Submit My Form" ]
    , Html.button [ Attr.id "default-type-button", Attr.form "my-form" ] [ Html.text "Submit My Form By Default" ]
    ]

submitBadFormDescendentView : Model -> Html Msg
submitBadFormDescendentView model =
  Html.div [ Attr.id "root" ]
    [ Html.form [ onSubmit HandleSubmit ]
      [ Html.input [ Attr.type_ "submit", Attr.form "my-formsss", Attr.name "Submit My Form" ] []
      , Html.button [ Attr.type_ "submit", Attr.form "my-formsss" ] [ Html.text "Submit My Form" ]
      , Html.button [ Attr.id "default-type-button", Attr.form "my-formsss" ] [ Html.text "Submit My Form By Default" ]
      ]
    ]

submitBadFormView : Model -> Html Msg
submitBadFormView model =
  Html.div [ Attr.id "root" ]
    [ Html.input [ Attr.type_ "submit", Attr.form "my-formsss", Attr.name "Submit My Form" ] []
    , Html.button [ Attr.type_ "submit", Attr.form "my-formsss" ] [ Html.text "Submit My Form" ]
    , Html.button [ Attr.id "default-type-button", Attr.form "my-formsss" ] [ Html.text "Submit My Form By Default" ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoInput input ->
      ( { model | name = input }, Cmd.none )
    DoKeyUp char ->
      ( { model | lastLetter = char }, Cmd.none )
    HandleCheck didCheck ->
      ( { model | isChecked = didCheck }, Cmd.none )
    HandleSubmit ->
      ( { model | isSubmitted = True }, Cmd.none )
    HandleClick ->
      ( model, Cmd.none )

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)
