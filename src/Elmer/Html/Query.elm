module Elmer.Html.Query exposing
  ( findElement
  , findElements
  )


import Elmer.Html.Types exposing (..)
import Elmer.Html.Element.Printer as ElementPrinter
import Elmer.Html.Selector.Printer as SelectorPrinter
import Elmer.Html.Element.Internal as Html_
import Elmer.Errors as Errors


findElement : HtmlTarget msg -> Result String (HtmlElement msg)
findElement (HtmlTarget selection) =
    selection.element
      |> Maybe.andThen (\rootElement ->
          find matchAllDescendants selection.selector rootElement
            |> List.head
        )
      |> Result.fromMaybe (queryErrorMessage selection)


findElements : HtmlTarget msg -> List (HtmlElement msg)
findElements (HtmlTarget selection) =
  selection.element
    |> Maybe.map (find matchAllDescendants selection.selector)
    |> Maybe.withDefault []


type alias ElementMatcher msg =
    HtmlSelectorGroup msg -> List (HtmlSelector msg) -> HtmlElement msg -> List (HtmlElement msg)


find : ElementMatcher msg -> HtmlSelectorGroup msg -> HtmlElement msg -> List (HtmlElement msg)
find matcher selector element =
  case selector of
    ElementWith selectors ->
        if List.isEmpty selectors then
            []
        else
            matcher selector selectors element
    DescendantsOf selectors next ->
        findWithin matcher selectors element
            |> List.concatMap (find matchAllDescendants next)
    ChildrenOf selectors next ->
        findWithin matcher selectors element
            |> List.concatMap (find matchElementOnly next)


findWithin : ElementMatcher msg -> List (HtmlSelector msg) -> HtmlElement msg -> List (HtmlElement msg)
findWithin matcher selectors element =
    find matcher (ElementWith selectors) element
        |> List.concatMap Html_.childElements


matchElementOnly : HtmlSelectorGroup msg -> List (HtmlSelector msg) -> HtmlElement msg -> List (HtmlElement msg)
matchElementOnly _ selectors element =
    if matches selectors element then
        [ element ]
    else
        []


matchAllDescendants : HtmlSelectorGroup msg -> List (HtmlSelector msg) -> HtmlElement msg -> List (HtmlElement msg)
matchAllDescendants selector selectors element =
    if matches selectors element then
        element ::
            descendantsThatMatch selector element
    else
        descendantsThatMatch selector element


descendantsThatMatch : HtmlSelectorGroup msg -> HtmlElement msg -> List (HtmlElement msg)
descendantsThatMatch selector element =
    Html_.childElements element
        |> List.concatMap (find matchAllDescendants selector)


matches : List (HtmlSelector msg) -> HtmlElement msg -> Bool
matches selectors element =
    List.map .predicate selectors
      |> List.foldl (\sel result ->
        case result of
            True ->
                sel element
            False ->
                False
      ) True


queryErrorMessage : Selection msg -> String
queryErrorMessage selection =
  elementToString selection.element
    |> Errors.elementNotFound (SelectorPrinter.printGroup selection.selector)
    |> Errors.print


elementToString : Maybe (HtmlElement msg) -> String
elementToString maybeElement =
  case maybeElement of
    Just element ->
      ElementPrinter.print element
    Nothing ->
      "<No Elements>"
