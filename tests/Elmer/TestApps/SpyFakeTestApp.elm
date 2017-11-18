module Elmer.TestApps.SpyFakeTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events

type alias Model =
  { name: String
  , number: Int
  }

type Msg
  = FetchName
  | FetchedName String

type alias Dependencies =
  { fetchName : (String -> Msg) -> String -> Cmd Msg
  , getNumber : String -> Int
  }

initialModel : Model
initialModel =
  { name = ""
  , number = 0
  }

view : Model -> Html Msg
view model =
  Html.div []
  [ Html.button [ Attr.id "fetch-name-button", Events.onClick FetchName ]
    [ Html.text "Click for name" ]
  , Html.div []
    [ Html.text <| "Name" ++ model.name ]
  , Html.div []
    [ Html.text <| "Your number is: " ++ toString model.number ]
  ]

update : Dependencies -> Msg -> Model -> (Model, Cmd Msg)
update deps message model =
  case message of
    FetchName ->
      ( model
      , deps.fetchName FetchedName "Cool Dude"
      )
    FetchedName name ->
      ( { model
        | name = name
        , number = deps.getNumber name
        }
      , Cmd.none
      )
