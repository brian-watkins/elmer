module Elmer.Context exposing
  ( defaultHtmlContext
  , defaultCommandContext
  , withSpies
  )

import Elmer.Http.Internal exposing (HttpRequest)
import Elmer.Spy.Internal exposing (Spy)
import Elmer.Runtime.Command as RuntimeCommand
import Elmer.Context.Internal exposing (..)
import Navigation
import Html exposing (Html)


defaultHtmlContext : model -> ViewFunction model msg -> UpdateFunction model msg -> Context model msg
defaultHtmlContext model view update =
  { model = model
  , view = view
  , update = update
  , targetSelector = Nothing
  , locationParser = Nothing
  , location = Nothing
  , httpRequests = []
  , deferredCommands = []
  , dummyCommands = []
  , subscriptions = Sub.none
  , spies = []
  , commandGenerator = Nothing
  , messages = []
  }


type alias CommandContextModel msg =
  { messages : List msg
  }

emptyView : model -> Html msg
emptyView model =
  Html.text ""

messageCollectorUpdate : msg -> model -> (model, Cmd msg)
messageCollectorUpdate msg model =
  ( model
  , RuntimeCommand.mapContext (\context ->
      { context | messages = msg :: context.messages }
    )
  )

defaultCommandContext : (() -> Cmd msg) -> Context {} msg
defaultCommandContext commandGenerator =
  defaultHtmlContext {} emptyView messageCollectorUpdate
    |> withCommandGenerator commandGenerator

withCommandGenerator : (() -> Cmd msg) -> Context model msg -> Context model msg
withCommandGenerator generator context =
  { context | commandGenerator = Just generator }


withSpies : List Spy -> Context model msg -> Context model msg
withSpies spies context =
  { context | spies = spies }
