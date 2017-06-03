module Elmer.Platform.Internal exposing
  ( Intention(..)
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  , mapStateCommand
  , generateCommand
  )

import Elmer.Context as Context exposing (Context)


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

mapStateCommand : (Context model msg -> Context model msg) -> Cmd msg
mapStateCommand mapper =
  toCmd "Elmer_MapState" mapper

generateCommand : (Context model msg -> Cmd msg) -> Cmd msg
generateCommand generator =
  toCmd "Elmer_Generate" generator
