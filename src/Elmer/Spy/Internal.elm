module Elmer.Spy.Internal exposing
  ( Spy(..)
  , Calls
  , install
  , installAll
  , uninstallAll
  , calls
  , batch
  )


type alias Calls =
  { name : String
  , calls : Int
  }

type Spy
  = Uninstalled (() -> Spy)
  | Active SpyValue
  | Error SpyValue
  | Batch (List Spy)

type SpyValue
  = SpyValue


install : String -> (() -> a) -> Spy
install name namingFunc =
  Native.Spy.install name namingFunc

calls : String -> List Spy -> Maybe Calls
calls name spies =
  List.filterMap (\spy ->
    case spy of
      Active spyValue ->
        let
          calls = Native.Spy.calls spyValue
        in
          if calls.name == name then
            Just calls
          else
            Nothing
      _ ->
        Nothing
  ) spies
    |> List.head

{-| Note: Calling a fake method on a batch spy is not supported
-}
batch : List Spy -> Spy
batch spies =
  Batch spies

installAll : List Spy -> List Spy
installAll spies =
  List.map (\spy ->
    case spy of
      Uninstalled installer ->
        installer () :: []
      Batch spies ->
        installAll spies
      _ ->
        spy :: []
  ) spies
    |> List.concat

uninstall : Spy -> Spy
uninstall spy =
  case spy of
    Active spyValue ->
      Native.Spy.uninstall spyValue
    _ ->
      spy

uninstallAll : List Spy -> List Spy
uninstallAll =
  List.map uninstall
