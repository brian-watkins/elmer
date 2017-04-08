module Elmer.Platform.Internal exposing
  ( Intention(..)
  , Spy(..)
  , Calls
  , SpyId
  , spyOn
  , callsForSpy
  , installSpies
  , batchSpy
  , clearSpies
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  , mapStateCommand
  , generateCommand
  )

import Elmer.Internal as Internal exposing (..)

type alias Calls =
  { name : String
  , calls : Int
  }

type alias SpyId =
  String

type Spy =
  Spy (() -> Maybe SpyId)

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

spyOn : String -> (() -> a) -> Maybe SpyId
spyOn name namingFunc =
  Native.Platform.spy name namingFunc

callsForSpy : String -> Maybe Calls
callsForSpy name =
  Native.Platform.callsForSpy name

{-| Note: Calling a fake method on a batch spy is not supported
-}
batchSpy : List Spy -> Spy
batchSpy overrides =
  Spy <|
    \() ->
      installSpies overrides

installSpies : List Spy -> Maybe SpyId
installSpies =
  List.foldl (\(Spy func) cur -> Maybe.andThen (\_ -> func ()) cur) (Just "")

clearSpies : ComponentState model msg -> ComponentState model msg
clearSpies stateResult =
  if Native.Platform.clearSpies () then
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
