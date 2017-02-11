module Elmer.TestApps.SubscriptionTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Time exposing (Time)

type alias Model =
  { time : Time
  , minuteTime : Time
  , childModel : ChildModel
  }

defaultModel : Model
defaultModel =
  { time = 0
  , minuteTime = 0
  , childModel = defaultChildModel
  }

type Msg
  = NewTime Time
  | NewMinute Time
  | Child ChildMsg

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "time" ] [ Html.text ( (formatTime model.time) ++ " seconds" ) ]
    , Html.div [ Attr.id "minute" ] [ Html.text ( (formatMinutes model.minuteTime) ++ " minutes" ) ]
    , Html.map Child (childView model.childModel)
    ]

formatTime : Time -> String
formatTime time =
  Time.inSeconds time
    |> toString

formatMinutes : Time -> String
formatMinutes time =
  Time.inMinutes time
    |> toString

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewTime time ->
      ( { model | time = time }, Cmd.none )
    NewMinute time ->
      ( { model | minuteTime = time }, Cmd.none )
    Child childMsg ->
      let
        ( childModel, childCmd ) = childUpdate childMsg model.childModel
      in
        ( { model | childModel = childModel }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second NewTime

batchedSubscriptions : Model -> Sub Msg
batchedSubscriptions model =
  Sub.batch
    [ Time.every Time.second NewTime
    , Time.every Time.minute NewMinute
    ]

mappedSubscriptions : Model -> Sub Msg
mappedSubscriptions model =
  Sub.map Child (childSubscriptions model.childModel)


-- Child Component

type ChildMsg
  = NewHour Time
  | NewMilli Time

type alias ChildModel =
  { hours: Time }

defaultChildModel : ChildModel
defaultChildModel =
  { hours = 0 }

childView : ChildModel -> Html ChildMsg
childView model =
  Html.div [ Attr.id "child" ]
    [ Html.div [ Attr.id "child-hours" ] [ Html.text ((formatHour model.hours) ++ " hours") ]
    ]

childUpdate : ChildMsg -> ChildModel -> ( ChildModel, Cmd ChildMsg )
childUpdate msg model =
  case msg of
    NewHour time ->
      ( { model | hours = time }, Cmd.none )
    NewMilli time ->
      ( model, Cmd.none )

formatHour : Time -> String
formatHour time =
  Time.inHours time
    |> toString

childSubscriptions : ChildModel -> Sub ChildMsg
childSubscriptions model =
  Sub.batch
    [ Time.every Time.hour NewHour
    , Time.every Time.millisecond NewMilli
    ]
