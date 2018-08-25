module Elmer.Context exposing
  ( Context
  , View(..)
  , Update
  , default
  , model
  , withModel
  , view
  , render
  , update
  , state
  , updateState
  , updateStateFor
  )

import Elmer.Runtime.Intention as Intention
import Html exposing (Html)
import Browser exposing (Document)


type View model msg
  = HtmlView (model -> Html msg)
  | DocumentView (model -> Document msg)


type alias Update model msg =
    msg -> model -> ( model, Cmd msg )


type Context model msg
  = Context (ContextInfo model msg)


type alias ContextInfo model msg =
  { model : Maybe model
  , view : View model msg
  , update : Update model msg
  , state : List (Cmd msg)
  }


default : View model msg -> Update model msg -> Context model msg
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


view : Context model msg -> View model msg
view (Context context) =
  context.view


render : Context model msg -> Maybe (Html msg)
render (Context context) =
  Maybe.map (htmlRenderable context) context.model


htmlRenderable : ContextInfo model msg -> model -> Html msg
htmlRenderable context =
  case context.view of
    HtmlView viewFunction ->
      viewFunction
    DocumentView viewFunction ->
      \modelValue -> 
        viewFunction modelValue
          |> .body
          |> Html.node "body" []


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