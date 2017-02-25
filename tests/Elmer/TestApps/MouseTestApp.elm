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
  )

type alias Model =
  { clicks : Int
  , doubleClicks : Int
  , mouseDowns : Int
  , mouseUps : Int
  , mouseEnters : Int
  , mouseLeaves : Int
  , mouseOvers : Int
  }

type Msg
  = DoClick
  | DoDoubleClick
  | DoMouseDown
  | DoMouseUp
  | DoMouseEnter
  | DoMouseLeave
  | DoMouseOver

defaultModel : Model
defaultModel =
  { clicks = 0
  , doubleClicks = 0
  , mouseDowns = 0
  , mouseUps = 0
  , mouseEnters = 0
  , mouseLeaves = 0
  , mouseOvers = 0
  }

view : Model -> Html Msg
view model =
  Html.div [Attr.id "root"]
    [ Html.div
      [ Attr.class "button"
      , onClick DoClick
      , onDoubleClick DoDoubleClick
      , onMouseDown DoMouseDown
      , onMouseUp DoMouseUp
      , onMouseEnter DoMouseEnter
      , onMouseLeave DoMouseLeave
      , onMouseOver DoMouseOver
      ] [ Html.text "Click me!" ]
    , Html.div [ Attr.id "click-counter" ] [ Html.text ((toString model.clicks) ++ " clicks!") ]
    ]

viewForMouseOver : Model -> Html Msg
viewForMouseOver model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.class "no-events" ] [ Html.text "no events" ]
    , Html.div [ Attr.id "event-parent", onMouseOver DoMouseOver ]
      [ Html.ul []
        [ Html.li [ Attr.attribute "data-option" "1" ] [ Html.text "Option 1" ]
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
