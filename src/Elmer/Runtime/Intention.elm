module Elmer.Runtime.Intention exposing
  ( Intention(..)
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  )


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
  Native.Intention.asIntention command

cmdValue : Cmd a -> b
cmdValue cmd =
  Native.Intention.intentionValue cmd

toCmd : String -> a -> Cmd msg
toCmd home data =
  Native.Intention.toIntention home data

subData : Sub msg -> Intention a msg subMsg
subData subscription =
  Native.Intention.asIntention subscription

subValue : Sub a -> b
subValue sub =
  Native.Intention.intentionValue sub

toSub : String -> a -> Sub msg
toSub home data =
  Native.Intention.toIntention home data
