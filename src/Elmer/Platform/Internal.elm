module Elmer.Platform.Internal exposing
  ( Intention(..)
  , Stub(..)
  , Spy
  , spyOn
  , spyData
  , stub
  , clearStubs
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  , installStubs
  , mapStateCommand
  , generateCommand
  )

import Elmer.Internal as Internal exposing (..)

type alias Spy =
  { name : String
  , calls : Int
  }

type Stub =
  Stub (() -> Bool)

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

spyOn : String -> (() -> a) -> Bool
spyOn name namingFunc =
  Native.Platform.spy name namingFunc

spyData : String -> Maybe Spy
spyData name =
  Native.Platform.spyData name

stub : (() -> a) -> (b -> c) -> Bool
stub namingFunc stubbingFunc =
  Native.Platform.swizzle namingFunc stubbingFunc

installStubs : List Stub -> Bool
installStubs =
  List.foldl (\(Stub func) cur -> cur && func ()) True

clearStubs : ComponentState model msg -> ComponentState model msg
clearStubs stateResult =
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

mapStateCommand : (Component model msg -> Component model msg) -> Cmd msg
mapStateCommand mapper =
  toCmd "Elmer_MapState" mapper

generateCommand : (Component model msg -> Cmd msg) -> Cmd msg
generateCommand generator =
  toCmd "Elmer_Generate" generator
