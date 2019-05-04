module Elmer.Program exposing
  ( givenElement
  , givenApplication
  , givenDocument
  , givenWorker
  , init
  )

{-| Functions for working with Elm programs.

# Test an Elm Html Sandbox or Element Program
@docs givenElement

# Test an Elm Html Application
@docs givenApplication

# Test an Elm Html Document
@docs givenDocument

# Test am Elm Worker Program
@docs givenWorker

# Initialize an Elm program
@docs init

-}

import Elmer exposing (TestState)
import Elmer.Context as Context exposing (View(..))
import Elmer.TestState as TestState
import Elmer.Navigation.Internal exposing (NavigationState(..))
import Elmer.Effects as Effects
import Elmer.Runtime as Runtime
import Browser exposing (UrlRequest, Document)
import Url exposing (Url)
import Html exposing (Html)



{-| Initialize a `TestState` with the basic requirements for a program created with
`Browser.sandbox` or `Browser.element`.

The arguments are:

1. View function
2. Update function

You'll need to call `Elmer.Program.init` with the program's `init` function to properly
start your test.
-}
givenElement : ( model -> Html msg )
    -> ( msg -> model -> ( model, Cmd msg ) )
    -> TestState model msg
givenElement view update =
  Context.default (HtmlView view) update
    |> TestState.with


{-| Initialize a `TestState` with the basic requirements for a program
created with `Browser.application`. 

The arguments are:

1. Function that maps a new `Browser.UrlRequest` to a `msg`
2. Function that maps a `Url` to a `msg` when the url changes
3. View function that results in a `Browser.Document`
4. Update function

You'll need to call `Elmer.Program.init` with the program's `init` function to properly
start your test.
-}
givenApplication : (UrlRequest -> msg) -> (Url -> msg) -> (model -> Document msg) -> (msg -> model -> (model, Cmd msg)) -> TestState model msg
givenApplication onUrlRequest onUrlChange view update =
  Context.default (DocumentView view) update
    |> Context.updateState (storeNavigationTaggersCommand onUrlRequest onUrlChange)
    |> TestState.with


storeNavigationTaggersCommand : (UrlRequest -> msg) -> (Url -> msg) -> Cmd msg
storeNavigationTaggersCommand onUrlRequest onUrlChange =
  Effects.push NavigationTaggers <|
    \_ ->
      { onUrlRequest = onUrlRequest
      , onUrlChange = onUrlChange
      }


{-| Initialize a `TestState` with the basic requirements for a program
created with `Browser.document`.

The arguments are:

1. View function that results in a `Browser.Document`
2. Update function.

You'll need to call `Elmer.Program.init` with the program's `init` function to properly
start your test.
-}
givenDocument : (model -> Document msg) -> (msg -> model -> (model, Cmd msg)) -> TestState model msg
givenDocument view update =
  Context.default (DocumentView view) update
    |> TestState.with


{-| Initialize a `TestState` with the basic requirements for
a headless worker program created with `Platform.worker`.

The argument is an update function.

You'll need to call `Elmer.Program.init` with the program's `init` function to properly
start your test.
-}
givenWorker : ( msg -> model -> ( model, Cmd msg ) ) -> TestState model msg
givenWorker update =
  Context.default (HtmlView emptyView) update
    |> TestState.with

emptyView : model -> Html msg
emptyView _ =
  Html.text ""


{-| Update the test context with the given model and Cmd.

Provide a function that calls a program's `init` function and returns a model and a command. 
The resuling model will become the current model for the system under test and the given command
will be executed. 

    Elmer.Program.givenDocument MyDocument.view MyDocument.update
      |> init (\() -> MyDocument.init)
      |> Elmer.Html.target
          << by [ id "title" ]
      |> Elmer.Html.expectElementExists

Note: If your test requires any spies, call `Spy.use` before your call to `init` so the spies will
be available whan the supplied function is evaluated.
-}
init : (() -> (model, Cmd msg)) -> TestState model msg -> TestState model msg
init initThunk =
  TestState.map <|
    \context ->
      let
        (initModel, initCommand) = initThunk ()
        updatedContext = Context.withModel initModel context
      in
        case Runtime.performCommand initCommand updatedContext of
          Ok initializeContext ->
            TestState.with initializeContext
          Err message ->
            TestState.failure message
