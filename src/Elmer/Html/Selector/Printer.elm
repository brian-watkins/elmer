module Elmer.Html.Selector.Printer exposing
  ( printGroup
  , print
  )

{-| Exposed for testing

@docs printGroup, print

-}

import Elmer.Html.Types exposing (HtmlSelectorGroup(..), HtmlSelector)

{-|
-}
printGroup : HtmlSelectorGroup msg -> String
printGroup selectorGroup =
  case selectorGroup of
    ElementWith selectors ->
      "by " ++ printSelectors selectors
    DescendantsOf selectors group ->
      "descendants of " ++ printSelectors selectors ++ " " ++ printGroup group
    ChildrenOf selectors group ->
      "children of " ++ printSelectors selectors ++ " " ++ printGroup group


printSelectors : List (HtmlSelector msg) -> String
printSelectors selectors =
  let
    selectorText =
      List.map print selectors
        |> String.join ", "
  in
    "[ " ++ selectorText ++ " ]"

{-|
-}
print : HtmlSelector msg -> String
print selector =
  selector.description