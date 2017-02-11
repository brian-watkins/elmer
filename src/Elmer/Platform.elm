module Elmer.Platform exposing
  ( Intention(..)
  , PlatformOverride
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  , override
  , batchOverride
  , mapWithOverrides
  )

import Elmer
import Elmer.Types exposing (..)

type Intention a msg subMsg
    = Leaf (LeafData a)
    | Tree (TreeData a subMsg msg)
    | Batch (List a)
    | Unknown

type alias TreeData a subMsg msg =
    { tree : a
    , tagger : subMsg -> msg
    }

type alias LeafData a =
    { intention : a
    , home : String
    }

type alias PlatformOverride =
  (() -> Bool)

override : (() -> a) -> (b -> c) -> PlatformOverride
override namingFunc overridingFunc =
  \() ->
    Native.Helpers.swizzle namingFunc overridingFunc

batchOverride : List PlatformOverride -> PlatformOverride
batchOverride overrides =
  \() ->
    executeOverrides overrides

mapWithOverrides : String -> List PlatformOverride -> (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
mapWithOverrides platformType overrides mapper =
  Elmer.map (\componentState ->
    if executeOverrides overrides then
      mapper componentState
        |> restore
    else
      "Failed to override " ++ platformType ++ "!"
        |> UpstreamFailure
        |> restore
  )

executeOverrides : List PlatformOverride -> Bool
executeOverrides overrides =
  List.foldl (\func cur -> cur && func ()) True overrides


restore : ComponentStateResult model msg -> ComponentStateResult model msg
restore stateResult =
  if Native.Helpers.restoreSwizzled () then
    stateResult
  else
    UpstreamFailure "Failed to restore swizzled functions! (This should never happen)"


cmdData : Cmd msg -> Intention a msg subMsg
cmdData command =
  Native.Helpers.asIntention command

cmdValue : Cmd a -> b
cmdValue cmd =
  Native.Helpers.intentionValue cmd

toCmd : String -> a -> Cmd msg
toCmd home data =
  Native.Helpers.toIntention home data

subData : Sub msg -> Intention a msg subMsg
subData subscription =
  Native.Helpers.asIntention subscription

subValue : Sub a -> b
subValue sub =
  Native.Helpers.intentionValue sub

toSub : String -> a -> Sub msg
toSub home data =
  Native.Helpers.toIntention home data
