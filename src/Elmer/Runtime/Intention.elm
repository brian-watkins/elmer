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
  Value.global "_Platform_leaf"


asIntention : v -> Intention v msg subMsg
asIntention value =
  -- let
      -- d = Elm.Kernel.Value.print "intention value" value
  -- in
  case Value.field "$" value of
    1 ->
      Leaf ({ intention = value, home = Value.field "k" value })
    2 ->
      Batch (Value.field "m" value)
    3 ->
      Tree ({ tree = Value.field "o" value, tagger = Value.field "n" value })
    unknownType ->
      "Unknown intention type: " ++ String.fromInt unknownType
        |> Debug.todo


-- REVISIT: Do we need to handle the case of 'batch' commands?
intentionValue : a -> b
intentionValue intention =
  case Value.field "$" intention of
    1 ->
      Value.field "l" intention
    3 ->
      Value.field "o" intention
        |> intentionValue
    unknownType ->
      "Unknown intention type: " ++ String.fromInt unknownType
        |> Debug.todo
