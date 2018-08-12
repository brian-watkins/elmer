module Elmer.TestApps.SubscriptionTestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Time exposing (Posix, utc)

type alias Model =
  { time : Posix
  , minuteTime : Posix
  , childModel : ChildModel
  }

defaultModel : Model
defaultModel =
  { time = Time.millisToPosix 0
  , minuteTime = Time.millisToPosix 0
  , childModel = defaultChildModel
  }

type Msg
  = NewTime Posix
  | NewMinute Posix
  | Child ChildMsg

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "root" ]
    [ Html.div [ Attr.id "time" ] [ Html.text ( (formatTime model.time) ++ " seconds" ) ]
    , Html.div [ Attr.id "minute" ] [ Html.text ( (formatMinutes model.minuteTime) ++ " minutes" ) ]
    , Html.map Child (childView model.childModel)
    ]

formatTime : Posix -> String
formatTime time =
  Time.toSecond utc time
    |> String.fromInt

formatMinutes : Posix -> String
formatMinutes time =
  Time.toMinute utc time
    |> String.fromInt

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
  Time.every 1000 NewTime

batchedSubscriptions : Model -> Sub Msg
batchedSubscriptions model =
  Sub.batch
    [ Time.every 1000 NewTime
    , Time.every (60 * 1000) NewMinute
    ]

mappedSubscriptions : Model -> Sub Msg
mappedSubscriptions model =
  Sub.map Child (childSubscriptions model.childModel)


-- Child Component

type ChildMsg
  = NewHour Posix
  | NewMilli Posix

type alias ChildModel =
  { hours: Posix }

defaultChildModel : ChildModel
defaultChildModel =
  { hours = Time.millisToPosix 0 }

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

formatHour : Posix -> String
formatHour time =
  Time.toHour utc time
    |> String.fromInt

childSubscriptions : ChildModel -> Sub ChildMsg
childSubscriptions model =
  Sub.batch
    [ Time.every (60 * 60 * 1000) NewHour
    , Time.every 1 NewMilli
    ]
