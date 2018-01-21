module Elmer.TestApps.HtmlKeyedTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)

type alias Model =
  { name : String }

type Msg
  = DoClick
  | DoSpecialClick String

defaultModel : Model
defaultModel =
  { name = "orange" }

view : Model -> Html Msg
view model =
  Html.div
    [ Attr.id "root", Events.onClick DoClick ]
    [ Keyed.ol [ Attr.id "fruit-list" ]
      [ ( "ol-1", Html.li [] [ Html.text "apple" ] )
      , ( "ol-2", Html.li [] [ Html.text "pear" ] )
      , ( "ol-3", Html.li [] [ Html.text model.name ] )
      ]
    ]


view2 : Model -> Html Msg
view2 model =
  Html.div
    [ Attr.id "root" ]
    [ Keyed.ol [ Attr.id "fruit-list" ]
      [ ( "ol-1", Html.li [] [ Html.text "apple" ] )
      , ( "ol-2", Html.li [ Attr.id "special-node", Events.onClick <| DoSpecialClick "popcorn" ] [ Html.text "pear" ] )
      , ( "ol-3", Html.li [] [ Html.text model.name ] )
      ]
    ]

viewLazyNode : Model -> Html Msg
viewLazyNode model =
  Html.div
    [ Attr.id "root", Events.onClick DoClick ]
    [ Keyed.ol [ Attr.id "fruit-list" ]
      [ ( "ol-1", Html.li [] [ Html.text "apple" ] )
      , ( "ol-2", Html.li [] [ lazy lazyLi model ] )
      , ( "ol-3", Html.li [] [ Html.text model.name ] )
      ]
    ]

lazyLi : Model -> Html Msg
lazyLi model =
  Html.div [ Attr.id "lazy-div" ]
  [ Html.text "chocolate"
  ]

lazyKeyedView : Model -> Html Msg
lazyKeyedView model =
  Html.div [ Attr.id "root" ]
  [ lazy lazyKeyed model ]

lazyKeyed : Model -> Html Msg
lazyKeyed model =
  Keyed.ol [ Attr.id "fruit-list" ]
    [ ( "ol-1", Html.li [] [ Html.text "apple" ] )
    , ( "ol-2", Html.li [] [ Html.text "grapes" ] )
    , ( "ol-3", Html.li [] [ Html.text model.name ] )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | name = "pineapple" }
      , Cmd.none
      )
    DoSpecialClick food ->
      ( { model | name = food }
      , Cmd.none
      )
