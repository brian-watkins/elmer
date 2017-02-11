module Elmer.Subscription exposing
  ( use
  , override
  , send
  , spy
  )

import Elmer
import Elmer.Types exposing (..)
import Elmer.Printer exposing (..)
import Elmer.Runtime as Runtime
import Elmer.Command as Command
import Elmer.Platform as Platform exposing (..)


type alias SubDescription a msg =
  { name : String
  , tagger : (a -> msg)
  }


override : (() -> a) -> (b -> c) -> PlatformOverride
override namingFunc overridingFunc =
  Platform.override namingFunc overridingFunc


use : List PlatformOverride -> (model -> Sub msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
use overrides subsFunc =
  Platform.mapWithOverrides "subscriptions" overrides (\componentState ->
    let
      subscription = subsFunc componentState.model
    in
      CurrentState { componentState | subscriptions = subscription }
  )


asSubscriptionData : Sub msg -> Intention (Sub msg) msg subMsg
asSubscriptionData sub =
  Platform.subData sub


describeSub : String -> (a -> msg) -> SubDescription a msg
describeSub name tagger =
  { name = name
  , tagger = tagger
  }


spy : String -> (a -> msg) -> Sub msg
spy name tagger =
  Platform.toSub "Elmer_Sub" (describeSub name tagger)


send : String -> a -> ComponentStateResult model msg -> ComponentStateResult model msg
send subName data =
  Elmer.map (\componentState ->
    case findSubDescription subName componentState.subscriptions of
      Just subDesc ->
        let
          command = subDesc.tagger data |> Command.stub
        in
          case Runtime.performCommand command componentState of
            Ok updatedState ->
              CurrentState updatedState
            Err message ->
              UpstreamFailure message

      Nothing ->
        let
          spies = subscriptionSpyNames componentState.subscriptions
        in
          if List.isEmpty spies then
            UpstreamFailure <| format
              [ message "No subscription spy found with name" subName
              , description "because there are no subscription spies"
              ]
          else
            UpstreamFailure <| format
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
