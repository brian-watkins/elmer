module Elmer.Platform.Subscription exposing
  ( with
  , send
  , fake
  )

{-| Functions for dealing with subscriptions during your test.

Elmer allows you to manage the effects of subscriptions yourself, so you can
describe the behavior of a component under whatever conditions you need.

To manage the effects of a subscription, you'll need to do three things.

1. Stub the function in your code that produces the subscription so that
it returns a fake subscription.

2. Use `with` to register the subscription during your test.

2. Use `send` to send data for that subscription to your component.

3. Enjoy.

Note: Elmer supports `Platform.Sub.batch` and `Platform.Sub.map` so you can use these in your
component as expected.

# Register Subscriptions
@docs with

# Create a Fake Subscription
@docs fake

# Send Data for a Subscription
@docs send

-}

import Elmer
import Elmer.ComponentState as ComponentState exposing (ComponentState)
import Elmer.Printer exposing (..)
import Elmer.Runtime as Runtime
import Elmer.Platform.Command as Command
import Elmer.Platform.Internal as Platform exposing (..)


type alias SubDescription a msg =
  { name : String
  , tagger : (a -> msg)
  }


{-| Register fake subscriptions for use during your test.

Suppose your component updates the view with the new time every second. To
get the time every second, in your code you'll need to create a `subscriptions`
function that returns `Time.every Time.second <tagger>`. To describe this behavior in your test, you
could do the following:

    let
      fakeSub = Elmer.Platform.spy "fake-time" (\_ -> Time.every)
        |> Elmer.Platform.andCallFake (\interval tagger ->
          Elmer.Platform.Subscription.fake "everySecond" tagger
        )
    in
      componentState
        |> Spy.use [ fakeSub ]
        |> with (\() -> Component.subscriptions)
        |> send "everySecond" 3000
        |> find "#current-time"
        |> expectElement (hasText "3 seconds")

-}
with : (() -> (model -> Sub msg)) -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
with subsThunk =
  ComponentState.map (\component ->
    let
      subscription = subsThunk () <| component.model
    in
      ComponentState.withComponent { component | subscriptions = subscription }
  )


describeSub : String -> (a -> msg) -> SubDescription a msg
describeSub name tagger =
  { name = name
  , tagger = tagger
  }

{-| Generate a fake subscription with an identifier and the appropriate tagger.

Use `Elmer.Platform.spy` along with `Elmer.Platform.andCallFake` to stub a function
that generates a subscription so that it returns a fake instead. Then, once
the faked subscription is registered using `with`,
you can `send` data on behalf of it subscription during your test.

Here's an example that creates a fake subscription for mouse ups, registers it
and sends some data through it.

    let
      subSpy = Elmer.Platform.spy "fake-ups" (\_ -> Mouse.ups)
        |> Elmer.Platform.andCallFake (\tagger ->
          Subscription.fake "mouseUps" tagger
        )
    in
      Elmer.componentState defaultModel view update
        |> Spy.use [ subSpy ]
        |> with (\() -> subscriptions)
        |> send "mouseUps" { x = 10, y = 50 }

-}
fake : String -> (a -> msg) -> Sub msg
fake name tagger =
  Platform.toSub "Elmer_Sub" (describeSub name tagger)

{-| Send data on behalf of the identified subscription.

Data sent via this function will be tagged accordingly and passed to
the component's `update` function for processing.
-}
send : String -> a -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
send subName data =
  ComponentState.map (\componentState ->
    case findSubDescription subName componentState.subscriptions of
      Just subDesc ->
        let
          command = subDesc.tagger data |> Command.fake
        in
          case Runtime.performCommand command componentState of
            Ok updatedState ->
              ComponentState.withComponent updatedState
            Err message ->
              ComponentState.failure message

      Nothing ->
        let
          spies = subscriptionSpyNames componentState.subscriptions
        in
          if List.isEmpty spies then
            ComponentState.failure <| format
              [ message "No subscription spy found with name" subName
              , description "because there are no subscription spies"
              ]
          else
            ComponentState.failure <| format
              [ message "No subscription spy found with name" subName
              , message "These are the current subscription spies" (String.join "\n" spies)
              ]
  )

subscriptionSpyNames : Sub msg -> List String
subscriptionSpyNames sub =
  case Platform.subData sub of
    Leaf leaf ->
      [ Platform.subValue leaf.intention |> .name ]
    Tree treeData ->
      subscriptionSpyNames treeData.tree
    Batch subs ->
      List.map subscriptionSpyNames subs
        |> List.concat
    Unknown ->
      []

findSubDescription : String -> Sub msg -> Maybe (SubDescription a msg)
findSubDescription subName sub =
  case Platform.subData sub of
    Leaf leaf ->
      let
        subDesc = Platform.subValue leaf.intention
      in
        if subDesc.name == subName then
          Just subDesc
        else
          Nothing
    Tree treeData ->
      findSubDescription subName treeData.tree
        |> Maybe.andThen (composeTagger treeData.tagger)
    Batch subs ->
      List.filterMap (findSubDescription subName) subs
        |> List.head
    Unknown ->
      Nothing

composeTagger : (subMsg -> msg) -> SubDescription a subMsg -> Maybe (SubDescription a msg)
composeTagger parentTagger subDesc =
  let
    composedTagger = parentTagger << subDesc.tagger
  in
    Just { subDesc | tagger = composedTagger }
