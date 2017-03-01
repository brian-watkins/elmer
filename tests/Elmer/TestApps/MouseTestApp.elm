module Elmer.TestApps.MouseTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing
  ( onClick
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  )

type alias Model =
  { clicks : Int
  , doubleClicks : Int
  , mouseDowns : Int
  , mouseUps : Int
  , mouseEnters : Int
  , mouseLeaves : Int
  , mouseOvers : Int
  , mouseOuts : Int
  }

type Msg
  = DoClick
  | DoDoubleClick
  | DoMouseDown
  | DoMouseUp
  | DoMouseEnter
  | DoMouseLeave
  | DoMouseOver
  | DoMouseOut

defaultModel : Model
defaultModel =
  { clicks = 0
  , doubleClicks = 0
  , mouseDowns = 0
  , mouseUps = 0
  , mouseEnters = 0
  , mouseLeaves = 0
  , mouseOvers = 0
  , mouseOuts = 0
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root", Attr.class "no-events"]
    [ Html.div
      [ Attr.class "button"
      , onClick DoClick
      , onDoubleClick DoDoubleClick
      , onMouseDown DoMouseDown
      , onMouseUp DoMouseUp
      , onMouseOver DoMouseOver
      , onMouseOut DoMouseOut
      ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((toString model.clicks) ++ " clicks!") ]
    ]

viewForMouseEnterLeave : Model -> Html Msg
viewForMouseEnterLeave model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.class "no-events" ] [ Html.text "no events" ]
    , Html.div [ Attr.id "event-parent", onMouseEnter DoMouseEnter, onMouseLeave DoMouseLeave ]
      [ Html.ul []
        [ Html.li
          [ Attr.attribute "data-option" "1"
          , onMouseEnter DoMouseEnter
          , onMouseLeave DoMouseLeave
          ] [ Html.text "Option 1" ]
        , Html.li [ Attr.attribute "data-option" "2" ] [ Html.text "Option 2" ]
        , Html.li [ Attr.attribute "data-option" "3" ] [ Html.text "Option 3" ]
        ]
      ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DoClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    DoDoubleClick ->
      ( { model | doubleClicks = model.doubleClicks + 1 }, Cmd.none )
    DoMouseDown ->
      ( { model | mouseDowns = model.mouseDowns + 1 }, Cmd.none )
    DoMouseUp ->
      ( { model | mouseUps = model.mouseUps + 1 }, Cmd.none )
    DoMouseEnter ->
      ( { model | mouseEnters = model.mouseEnters + 1 }, Cmd.none )
    DoMouseLeave ->
      ( { model | mouseLeaves = model.mouseLeaves + 1 }, Cmd.none )
    DoMouseOver ->
      ( { model | mouseOvers = model.mouseOvers + 1 }, Cmd.none )
    DoMouseOut ->
      ( { model | mouseOuts = model.mouseOuts + 1 }, Cmd.none )
