module Elmer.Runtime.Intention exposing
  ( Intention(..)
  , cmdValue
  , subValue
  , toCmd
  , toSub
  , cmdData
  , subData
  )

import Json.Decode as Json
import Elmer.Value as Value
import Elmer.Value.Native as Native

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
cmdData cmd =
  decode (intentionDecoder cmd) cmd


cmdValue : Cmd a -> b
cmdValue =
  decode intentionValueDecoder


subData : Sub msg -> Intention (Sub msg) msg subMsg
subData sub =
  decode (intentionDecoder sub) sub


subValue : Sub a -> b
subValue =
  decode intentionValueDecoder


decode : Json.Decoder a -> v -> b
decode decoder value =
  case Value.decode decoder value of
    Ok v ->
      v
    Err msg ->
      "Could not decode intention value: " ++ Json.errorToString msg
        |> Debug.todo


toCmd : String -> a -> Cmd msg
toCmd =
  toIntention


toSub : String -> a -> Sub msg
toSub =
  toIntention


toIntention : String -> a -> b
toIntention =
  Native.global "_Platform_leaf"


intentionDecoder : v -> Json.Decoder (Intention v msg subMsg)
intentionDecoder value =
  Native.constructor
    |> Json.andThen (\ctor ->
      case ctor of
        1 ->
          Native.field "k"
            |> Json.map (\home -> 
              Leaf { intention = value, home = home }
            )
        2 ->
          Native.field "m"
            |> Json.map Batch
        3 ->
          Json.map2 (\tree tagger ->
            Tree ({ tree = tree, tagger = tagger })
          )
            (Native.field "o")
            (Native.field "n")
        unknownType ->
          "Unknown intention type: " ++ String.fromInt unknownType
            |> Debug.todo      
    )
  

intentionValueDecoder : Json.Decoder (a -> b)
intentionValueDecoder =
  Native.constructor
    |> Json.andThen (\ctor ->
        case ctor of
          1 ->
            Native.field "l"
          3 ->
            Json.field "o" intentionValueDecoder
          unknownType ->
            "Unknown intention type: " ++ String.fromInt unknownType
              |> Debug.todo  
    )
