module Elmer.TestApps.TripleComponentTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


-- Models

type alias Model =
  { childModel: ChildModel
  }

type alias ChildModel =
  { grandChildModel: GrandChildModel
  }

type alias GrandChildModel =
  { name: String
  }

defaultModel : Model
defaultModel =
  { childModel = defaultChildModel
  }

defaultChildModel : ChildModel
defaultChildModel =
  { grandChildModel = defaultGrandChildModel
  }

defaultGrandChildModel : GrandChildModel
defaultGrandChildModel =
  { name = "Not Clicked"
  }

-- Msgs

type Msg =
  ChildMsgWrapper ChildMsg

type ChildMsg =
  GrandChildMsgWrapper GrandChildMsg

type GrandChildMsg =
  HandleClick String

-- Updates

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChildMsgWrapper childMsg ->
      let
        (childModel, childCmd) = childUpdate childMsg model.childModel
      in
        ( { model | childModel = childModel }, Cmd.map ChildMsgWrapper childCmd )

childUpdate : ChildMsg -> ChildModel -> (ChildModel, Cmd ChildMsg)
childUpdate msg model =
  case msg of
    GrandChildMsgWrapper grandChildMsg ->
      let
        (grandChildModel, grandChildCmd) = grandChildUpdate grandChildMsg model.grandChildModel
      in
        ( { model | grandChildModel = grandChildModel }, Cmd.map GrandChildMsgWrapper grandChildCmd )

grandChildUpdate : GrandChildMsg -> GrandChildModel -> (GrandChildModel, Cmd GrandChildMsg)
grandChildUpdate msg model =
  case msg of
    HandleClick name ->
      ( { model | name = name }, Cmd.none )

-- Views

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "parent-view" ]
    [ Html.map ChildMsgWrapper <| childView model.childModel ]

childView : ChildModel -> Html ChildMsg
childView model =
  Html.div [ Attr.id "child-view" ]
    [ Html.map GrandChildMsgWrapper <| grandChildView model.grandChildModel ]

grandChildView : GrandChildModel -> Html GrandChildMsg
grandChildView model =
  Html.div [ Attr.id "grand-child-view" ]
    [ Html.div [ Attr.id "grand-child-name" ]
      [ Html.text model.name ]
    , Html.button [ onClick <| HandleClick "Handled Click" ]
      [ Html.text "Click Me"]
    ]
