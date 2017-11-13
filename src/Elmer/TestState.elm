module Elmer.TestState exposing
  ( TestState
  , create
  , createWithCommand
  , map
  , mapWithoutSpies
  , mapToExpectation
  , with
  , failure
  )

import Elmer.Context as Context
import Elmer.Context.Internal exposing (..)
import Elmer.Spy.Internal as Spy_ exposing (Spy)
import Elmer.Runtime as Runtime
import Expect
import Html exposing (Html)

type TestState model msg
    = Ready (Context model msg)
    | Failed String

create : model -> ViewFunction model msg -> UpdateFunction model msg -> TestState model msg
create model view update =
  Context.defaultHtmlContext model view update
    |> with

createWithCommand : (() -> Cmd msg) -> TestState {} msg
createWithCommand commandGenerator =
  Context.defaultCommandContext commandGenerator
    |> with

with : Context model msg -> TestState model msg
with context =
  Ready context

failure : String -> TestState model msg
failure message =
  Failed message


abstractMap : (String -> a) -> (Context model msg -> a) -> TestState model msg -> a
abstractMap failureMapper mapper testState =
  case testState of
    Ready context ->
      mapper context
    Failed message ->
      failureMapper message


map : (Context model msg -> TestState model msg) -> TestState model msg -> TestState model msg
map mapper =
  abstractMap Failed
    (\context ->
      let
        contextWithSpies = Context.withSpies (Spy_.activate context.spies) context
      in
        mapper contextWithSpies
          |> updateComponentWithDeactivatedSpies contextWithSpies
    )

mapWithoutSpies : (Context model msg -> TestState model msg) -> TestState model msg -> TestState model msg
mapWithoutSpies mapper =
  abstractMap Failed
    (\context ->
      mapper context
    )

updateComponentWithDeactivatedSpies : Context model msg -> TestState model msg -> TestState model msg
updateComponentWithDeactivatedSpies contextWithSpies =
  abstractMap
    (\message ->
      Failed message
        |> deactivateSpies contextWithSpies
    )
    (\context ->
      context
        |> Context.withSpies (Spy_.deactivate context.spies)
        |> with
    )


mapToExpectation : (Context model msg -> Expect.Expectation) -> TestState model msg -> Expect.Expectation
mapToExpectation mapper =
  abstractMap Expect.fail
    (\context ->
      let
        contextWithSpies = Context.withSpies (Spy_.activate context.spies) context
      in
        case contextWithSpies.commandGenerator of
          Just generator ->
            case Runtime.performCommand (generator ()) contextWithSpies of
              Ok resolvedContext ->
                mapper resolvedContext
                  |> deactivateSpies resolvedContext
              Err errorMessage ->
                Expect.fail errorMessage
                  |> deactivateSpies contextWithSpies
          Nothing ->
            mapper contextWithSpies
              |> deactivateSpies contextWithSpies
    )

deactivateSpies : Context model msg -> a -> a
deactivateSpies context subject =
  let
    uninstalled = Spy_.deactivate context.spies
  in
    subject
