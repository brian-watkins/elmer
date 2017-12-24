module Elmer.Runtime.Intention exposing
  ( Intention(..)
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  )

import Elmer.Value as Value

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


cmdData : Cmd msg -> Intention (Cmd msg) msg subMsg
cmdData =
  asIntention

cmdValue : Cmd a -> b
cmdValue =
  intentionValue

toCmd : String -> a -> Cmd msg
toCmd =
  toIntention

subData : Sub msg -> Intention (Sub msg) msg subMsg
subData =
  asIntention

subValue : Sub a -> b
subValue =
  intentionValue

toSub : String -> a -> Sub msg
toSub =
  toIntention


toIntention : String -> a -> b
toIntention =
  Value.global "_elm_lang$core$Native_Platform"
    |> Value.field "leaf"


asIntention : v -> Intention v msg subMsg
asIntention value =
  case Value.field "type" value of
    "leaf" ->
      Leaf ({ intention = value, home = Value.field "home" value })
    "map" ->
      Tree ({ tree = Value.field "tree" value, tagger = Value.field "tagger" value })
    "node" ->
      Batch (Value.field "branches" value)
    unknownType ->
      "Unknown intention type" ++ unknownType
        |> Debug.crash


intentionValue : int -> val
intentionValue int =
  if Value.field "type" int == "map" then
    Value.field "tree" int
      |> intentionValue
  else
    Value.field "value" int
