module Elmer.ComponentState exposing
  ( ComponentState
  , create
  , abstractMap
  , map
  , mapToExpectation
  , withComponent
  , failure
  )

import Elmer.Component exposing (..)
import Elmer.Spy.Internal as Spy_
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
    }

withComponent : Component model msg -> ComponentState model msg
withComponent component =
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
  abstractMap Failed mapper


mapToExpectation : (Component model msg -> Expect.Expectation) -> ComponentState model msg -> Expect.Expectation
mapToExpectation mapper =
  abstractMap Expect.fail (\component ->
    mapper component
      |> Spy_.clearSpies
  )
