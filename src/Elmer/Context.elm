module Elmer.Context exposing
  ( Context
  , ViewFunction
  , UpdateFunction
  , default
  , model
  , withModel
  , render
  , update
  , state
  , updateState
  , updateStateFor
  )

import Html exposing (Html)
import Elmer.Runtime.Intention as Intention


type alias ViewFunction model msg =
    model -> Html msg

type alias UpdateFunction model msg =
    msg -> model -> ( model, Cmd msg )

type Context model msg
  = Context
    { model : Maybe model
    , view : ViewFunction model msg
    , update : UpdateFunction model msg
    , state : List (Cmd msg)
    }


default : ViewFunction model msg -> UpdateFunction model msg -> Context model msg
default viewFunction updateFunction =
  Context
    { model = Nothing
    , view = viewFunction
    , update = updateFunction
    , state = []
    }


model : Context model msg -> Maybe model
model (Context context) =
  context.model


withModel : model -> Context model msg -> Context model msg
withModel modelValue (Context context) =
  Context
    { context | model = Just modelValue }


render : Context model msg -> Maybe (Html msg)
render (Context context) =
  Maybe.map context.view context.model


update : msg -> Context model msg -> Maybe (Context model msg, Cmd msg)
update message (Context context) =
  Maybe.map (\modelValue ->
    let
        ( updatedModel, command ) =
            context.update message modelValue

        updatedContext =
            { context | model = Just updatedModel }
    in
        ( Context updatedContext, command )
  ) context.model


state : typeId -> Context model msg -> Maybe a
state typeId (Context context) =
  context.state
    |> List.filter (\cmd -> typeId == (Intention.cmdValue cmd |> .typeId))
    |> List.map (\cmd -> Intention.cmdValue cmd |> .mapper)
    |> List.foldl (\mapper val -> Just <| mapper val) Nothing


updateState : Cmd msg -> Context model msg -> Context model msg
updateState command (Context context) =
  Context
    { context | state = context.state ++ [ command ] }

updateStateFor : Context model msg -> Cmd msg -> Context model msg
updateStateFor context command =
  updateState command context