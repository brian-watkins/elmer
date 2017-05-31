module Elmer.TestApps.SpyTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

type alias Model =
  { name : String
  , anotherName : String
  }

type Msg
  = HandleClick
  | HandleMultiArgClick
  | SuccessiveArgClick

defaultModel : Model
defaultModel =
  { name = "Cool Person"
  , anotherName = "Super"
  }

view : Model -> Html Msg
view model =
  Html.div
    [ Attr.id "root", Attr.class "styled no-events" ]
    [ Html.div [ Attr.id "title" ] [ Html.text <| titleText "Some Title" ]
    , Html.div [ Attr.id "button", Events.onClick HandleClick ] [ Html.text "Click me to clear!" ]
    , Html.div [ Attr.id "multi-arg-button", Events.onClick HandleMultiArgClick ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "successive-arg-button", Events.onClick SuccessiveArgClick ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "name" ] [ Html.text <| "Name: " ++ model.name ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( clearName "Default Name" model, Cmd.none )
    HandleMultiArgClick ->
      ( { model | name = combineNames "Dr." "Awesome" "Dude" }, Cmd.none )
    SuccessiveArgClick ->
      let
        combineFunc = combineNames "Mrs." "Funny"
      in
        ( { model | name = combineFunc "Animal" }, Cmd.none )

titleText : String -> String
titleText name =
  "A Title: " ++ name

combineNames : String -> String -> String -> String
combineNames kind firstName lastName =
  kind ++ firstName ++ lastName

clearName : String -> Model -> Model
clearName default model =
  { model | name = default }

type alias TestRecord =
  { kind : String
  , duration : Float
  }

type FunKind
  = Flower String
  | Fruit String
  | Game String

type alias Flags =
  { name : String
  , times : Int
  , floatArg : Float
  , boolArg : Bool
  , recordArg : TestRecord
  , unionTypeArg : FunKind
  , unionTypeTagger : (String -> FunKind)
  }

makeModel : String -> Int -> Float -> Bool -> TestRecord -> FunKind -> (String -> FunKind) -> Model
makeModel name times floatArg boolArg recordArg unionArg tagger =
  { name = name
  , anotherName = name
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( makeModel flags.name flags.times flags.floatArg flags.boolArg flags.recordArg flags.unionTypeArg flags.unionTypeTagger, Cmd.none )
