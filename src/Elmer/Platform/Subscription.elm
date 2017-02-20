module Elmer.Platform.Subscription exposing
  ( use
  , override
  , send
  , spy
  )

{-| Functions for dealing with subscriptions during your test.

Elmer allows you to manage the effects of subscriptions yourself, so you can
describe the behavior of a component under whatever conditions you need.

To manage the effects of a subscription, you'll need to do three things.

1. Override the function in your code that produces the subscription and replace
it with a function that returns a subscription spy.

2. Send data for that subscription to your component.

3. Enjoy.

Note: Elmer supports `Platform.Sub.batch` and `Platform.Sub.map` so you can use these in your
component as expected.

# Override a Subscription
@docs spy, override, use

# Send Data for a Subscription
@docs send

-}

import Elmer
import Elmer.Internal as Internal exposing (..)
import Elmer.Printer exposing (..)
import Elmer.Runtime as Runtime
import Elmer.Platform.Command as Command
import Elmer.Platform as Platform exposing (..)


type alias SubDescription a msg =
  { name : String
  , tagger : (a -> msg)
  }

{-| Override a function that generates a subscription.

The first argument is a function that simply returns the function you want to
override. The second argument is a function with the same signature as the function
to override. It should return a subscription spy, so that Elmer will know what to do.

Note: This function merely creates a description of the override; the function
is not actually overridden until you call `Subscription.use`.

You could override `Time.every` with a subscription spy like so:

    override (\_ -> Time.every) (\interval tagger ->
      spy "everyTimeInterval" tagger
    )

-}
override : (() -> a) -> (b -> c) -> Elmer.PlatformOverride
override namingFunc overridingFunc =
  Platform.override namingFunc overridingFunc


{-| Register subscription spies for use during your test, given the list of
overrides and a function that produces the component's subscriptions.

Suppose your component updates the view with the new time every second. To
get the time every second, in your code you'll need to create a `subscriptions`
function that returns `Time.every Time.second <tagger>`. To describe this behavior in your test, you
could do the following:

    let
      subOverride = override (\_ -> Time.every) (\interval tagger ->
        spy "everySecond" tagger
      )
    in
      componentState
        |> use [ subOverride ] Component.subscriptions
        |> send "everySecond" 3000
        |> find "#current-time"
        |> expectElement (hasText "3 seconds")

-}
use : List Elmer.PlatformOverride -> (model -> Sub msg) -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
use overrides subsFunc =
  Platform.mapWithOverrides "subscriptions" overrides (\componentState ->
    let
      subscription = subsFunc componentState.model
    in
      Ready { componentState | subscriptions = subscription }
  )


asSubscriptionData : Sub msg -> Intention (Sub msg) msg subMsg
asSubscriptionData sub =
  Platform.subData sub


describeSub : String -> (a -> msg) -> SubDescription a msg
describeSub name tagger =
  { name = name
  , tagger = tagger
  }

{-| Generate a fake subscription with an identifier and the appropriate tagger.

Once a spy is registered via the `use` function, you can
`send` data on behalf of the subscription during your test.
-}
spy : String -> (a -> msg) -> Sub msg
spy name tagger =
  Platform.toSub "Elmer_Sub" (describeSub name tagger)

{-| Send data on behalf of the identified subscription.

Data sent via this function will be tagged accordingly and passed to
the component's `update` function for processing.
-}
send : String -> a -> Elmer.ComponentState model msg -> Elmer.ComponentState model msg
send subName data =
  Internal.map (\componentState ->
    case findSubDescription subName componentState.subscriptions of
      Just subDesc ->
        let
          command = subDesc.tagger data |> Command.stub
        in
          case Runtime.performCommand command componentState of
            Ok updatedState ->
              Ready updatedState
            Err message ->
              Failed message

      Nothing ->
        let
          spies = subscriptionSpyNames componentState.subscriptions
        in
          if List.isEmpty spies then
            Failed <| format
              [ message "No subscription spy found with name" subName
              , description "because there are no subscription spies"
              ]
          else
            Failed <| format
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
