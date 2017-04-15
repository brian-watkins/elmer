module Elmer.Spy.Internal exposing
  ( Spy(..)
  , Calls
  , SpyId
  , spyOn
  , callsForSpy
  , installSpies
  , batch
  , clearSpies
  )


type alias Calls =
  { name : String
  , calls : Int
  }

type alias SpyId =
  String

type Spy =
  Spy (() -> Maybe SpyId)


spyOn : String -> (() -> a) -> Maybe SpyId
spyOn name namingFunc =
  Native.Spy.spy name namingFunc

callsForSpy : String -> Maybe Calls
callsForSpy name =
  Native.Spy.callsForSpy name

{-| Note: Calling a fake method on a batch spy is not supported
-}
batch : List Spy -> Spy
batch overrides =
  Spy <|
    \() ->
      installSpies overrides

installSpies : List Spy -> Maybe SpyId
installSpies =
  List.foldl (\(Spy func) cur -> Maybe.andThen (\_ -> func ()) cur) (Just "")

clearSpies : a -> a
clearSpies subject =
  if Native.Spy.clearSpies () then
    subject
  else
    Debug.crash "Failed to clear spies! This should never happen!?"
