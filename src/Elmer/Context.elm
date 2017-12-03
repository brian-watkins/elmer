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
  )

import Html exposing (Html)
import Elmer.Runtime.Intention as Intention


type alias ViewFunction model msg =
    model -> Html msg

type alias UpdateFunction model msg =
    msg -> model -> ( model, Cmd msg )

type Context model msg
  = Context
    { model : model
    , view : ViewFunction model msg
    , update : UpdateFunction model msg
    , state : List (Cmd msg)
    }


default : model -> ViewFunction model msg -> UpdateFunction model msg -> Context model msg
default model view update =
  Context
    { model = model
    , view = view
    , update = update
    , state = []
    }


model : Context model msg -> model
model (Context context) =
  context.model


withModel : model -> Context model msg -> Context model msg
withModel model (Context context) =
  Context
    { context | model = model }


render : Context model msg -> Html msg
render (Context context) =
  context.view context.model


update : msg -> Context model msg -> ( Context model msg, Cmd msg )
update message (Context context) =
  let
      ( updatedModel, command ) =
          context.update message context.model

      updatedContext =
          { context | model = updatedModel }
  in
      ( Context updatedContext, command )


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
