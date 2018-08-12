module Elmer.TestApps.LazyTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy exposing (lazy, lazy2, lazy3)


type alias Model =
  { name : String }


type Msg
  = DoClick
  | DoSpecialClick String


defaultModel : Model
defaultModel =
  { name = "Cool Person" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | name = "Awesome Person" }, Cmd.none )
    DoSpecialClick modifier ->
      ( { model | name = modifier ++ " " ++ model.name }, Cmd.none )


view : Model -> Html Msg
view model =
  Html.div
    [ Attr.id "root", Attr.class "styled no-events", Events.onClick DoClick ]
    [ Html.div [ Attr.id "name-text" ]
      [ lazy subView model ]
    ]


subView : Model -> Html Msg
subView model =
  Html.div [ Attr.id "lazy-div" ]
  [ Html.text <| "Some name: " ++ model.name
  ]


lazyView2 : Model -> Html Msg
lazyView2 model =
  Html.div
    [ Attr.id "root", Attr.class "styled no-events" ]
    [ lazy2 subView2 model "bowling" ]


subView2 : Model -> String -> Html Msg
subView2 model activity =
  Html.div [ Attr.id "lazy-div", Events.onClick <| DoSpecialClick "Happy" ]
  [ Html.text <| model.name ++ " likes " ++ activity
  ]


lazyView3 : Model -> Html Msg
lazyView3 model =
  Html.div
    [ Attr.id "root", Attr.class "styled no-events" ]
    [ lazy3 subView3 model "bowling" 5 ]


subView3 : Model -> String -> Int -> Html Msg
subView3 model activity times =
  model.name ++ " likes " ++ activity ++ " " ++ (String.fromInt times) ++ " times a week!"
    |> Html.text
