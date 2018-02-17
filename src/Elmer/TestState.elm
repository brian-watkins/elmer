module Elmer.TestState exposing
  ( TestState
  , TestStateExtension(..)
  , create
  , map
  , mapWithoutSpies
  , mapToExpectation
  , with
  , failure
  )

import Elmer.Context as Context exposing (..)
import Elmer.Spy.Internal as Spy_ exposing (Spy)
import Expect


type TestState model msg
    = Ready (Context model msg)
    | Failed String

type TestStateExtension
  = MapBeforeExpectationExtension


create : model -> ViewFunction model msg -> UpdateFunction model msg -> TestState model msg
create model view update =
  Context.default model view update
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
  abstractMap Failed <|
    spyMapExtension mapper


spyMapExtension : (Context model msg -> TestState model msg) -> Context model msg -> TestState model msg
spyMapExtension mapper context =
  let
    contextWithSpies = Spy_.withSpies (Spy_.activate <| Spy_.spiesFrom context) context
  in
    mapper contextWithSpies
      |> testStateWithDeactivatedSpies contextWithSpies


testStateWithDeactivatedSpies : Context model msg -> TestState model msg -> TestState model msg
testStateWithDeactivatedSpies contextWithSpies =
  abstractMap
    (\message ->
      Failed message
        |> deactivateSpies contextWithSpies
    )
    (\context ->
      context
        |> Spy_.withSpies (Spy_.deactivate <| Spy_.spiesFrom context)
        |> with
    )


mapWithoutSpies : (Context model msg -> TestState model msg) -> TestState model msg -> TestState model msg
mapWithoutSpies mapper =
  abstractMap Failed <|
    \context ->
      mapper context


mapToExpectation : (Context model msg -> Expect.Expectation) -> TestState model msg -> Expect.Expectation
mapToExpectation mapper testState =
  mapBeforeExpectation testState
    |> abstractMap Expect.fail (spyExpectationExtension <| mapper)


mapBeforeExpectation : TestState model msg -> TestState model msg
mapBeforeExpectation =
  map <|
    \context ->
      Context.state MapBeforeExpectationExtension context
        |> Maybe.withDefault []
        |> List.foldr map (with context)


spyExpectationExtension : (Context model msg -> Expect.Expectation) -> Context model msg -> Expect.Expectation
spyExpectationExtension mapper context =
  let
    contextWithSpies =
      Spy_.withSpies (Spy_.activate <| Spy_.spiesFrom context) context
  in
    mapper contextWithSpies
      |> deactivateSpies contextWithSpies


deactivateSpies : Context model msg -> a -> a
deactivateSpies context subject =
  let
    uninstalled = Spy_.deactivate <| Spy_.spiesFrom context
  in
    subject
