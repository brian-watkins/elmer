module Elmer.ComponentState exposing
  ( ComponentState
  , create
  , map
  , mapWithoutSpies
  , mapToExpectation
  , with
  , failure
  )

import Elmer.Component exposing (..)
import Elmer.Spy.Internal as Spy_ exposing (Spy)
import Expect
import Html exposing (Html)

type ComponentState model msg
    = Ready (Component model msg)
    | Failed String

create : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentState model msg
create model view update =
  Ready
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
    }

with : Component model msg -> ComponentState model msg
with component =
  Ready component

failure : String -> ComponentState model msg
failure message =
  Failed message


abstractMap : (String -> a) -> (Component model msg -> a) -> ComponentState model msg -> a
abstractMap failureMapper mapper componentState =
  case componentState of
    Ready component ->
      mapper component
    Failed message ->
      failureMapper message


map : (Component model msg -> ComponentState model msg) -> ComponentState model msg -> ComponentState model msg
map mapper =
  abstractMap Failed
    (\component ->
      let
        componentWithSpies =
          { component
          | spies = Spy_.activate component.spies
          }
      in
        mapper componentWithSpies
          |> updateComponentWithDeactivatedSpies componentWithSpies
    )

mapWithoutSpies : (Component model msg -> ComponentState model msg) -> ComponentState model msg -> ComponentState model msg
mapWithoutSpies mapper =
  abstractMap Failed
    (\component ->
      mapper component
    )

updateComponentWithDeactivatedSpies : Component model msg -> ComponentState model msg -> ComponentState model msg
updateComponentWithDeactivatedSpies componentWithSpies =
  abstractMap
    (\message ->
      Failed message
        |> deactivateSpies componentWithSpies
    )
    (\component ->
      with
        { component
        | spies = Spy_.deactivate component.spies
        }
    )


mapToExpectation : (Component model msg -> Expect.Expectation) -> ComponentState model msg -> Expect.Expectation
mapToExpectation mapper =
  abstractMap Expect.fail
    (\component ->
      let
        componentWithSpies =
          { component
          | spies = Spy_.activate component.spies
          }
      in
        mapper componentWithSpies
          |> deactivateSpies componentWithSpies
    )

deactivateSpies : Component model msg -> a -> a
deactivateSpies component subject =
  let
    uninstalled = Spy_.deactivate component.spies
  in
    subject
