module Elmer.Platform exposing
  ( Intention(..)
  , PlatformOverride(..)
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

import Elmer.Internal as Internal exposing (..)

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

type PlatformOverride =
  PlatformOverride (() -> Bool)

override : (() -> a) -> (b -> c) -> PlatformOverride
override namingFunc overridingFunc =
  PlatformOverride <|
    \() ->
      Native.Platform.swizzle namingFunc overridingFunc

batchOverride : List PlatformOverride -> PlatformOverride
batchOverride overrides =
  PlatformOverride <|
    \() ->
      executeOverrides overrides

mapWithOverrides : String -> List PlatformOverride -> (Component model msg -> ComponentState model msg) -> ComponentState model msg -> ComponentState model msg
mapWithOverrides platformType overrides mapper =
  Internal.map (\componentState ->
    if executeOverrides overrides then
      mapper componentState
        |> restore
    else
      "Failed to override " ++ platformType ++ "!"
        |> Failed
        |> restore
  )

executeOverrides : List PlatformOverride -> Bool
executeOverrides overrides =
  List.foldl (\(PlatformOverride func) cur -> cur && func ()) True overrides


restore : ComponentState model msg -> ComponentState model msg
restore stateResult =
  if Native.Platform.restoreSwizzled () then
    stateResult
  else
    Failed "Failed to restore swizzled functions! (This should never happen)"


cmdData : Cmd msg -> Intention a msg subMsg
cmdData command =
  Native.Platform.asIntention command

cmdValue : Cmd a -> b
cmdValue cmd =
  Native.Platform.intentionValue cmd

toCmd : String -> a -> Cmd msg
toCmd home data =
  Native.Platform.toIntention home data

subData : Sub msg -> Intention a msg subMsg
subData subscription =
  Native.Platform.asIntention subscription

subValue : Sub a -> b
subValue sub =
  Native.Platform.intentionValue sub

toSub : String -> a -> Sub msg
toSub home data =
  Native.Platform.toIntention home data
