module Elmer.Html.Query exposing
  ( findElement
  , findElements
  )


import Elmer.Html.Types exposing (..)
import Elmer.Html.Printer as ElementPrinter
import Elmer.Html.Internal as Html_
import Elmer.Errors as Errors


findElement : HtmlTarget msg -> Result String (HtmlElement msg)
findElement (HtmlTarget selection) =
    selection.element
      |> Maybe.andThen (\rootElement ->
          findAll selection.selector rootElement
            |> List.head
        )
      |> Result.fromMaybe (queryErrorMessage selection)


findElements : HtmlTarget msg -> List (HtmlElement msg)
findElements (HtmlTarget selection) =
  selection.element
    |> Maybe.map (findAll selection.selector)
    |> Maybe.withDefault []


findAll : HtmlSelectorGroup msg -> HtmlElement msg -> List (HtmlElement msg)
findAll selector element =
  case selector of
    Batch selectors ->
        case selectors of
            [] ->
                []
            sels ->
                if matchesAll sels element then
                    element ::
                        matchingDescendants selector element
                else
                    matchingDescendants selector element
    Descendants selectors next ->
      findAll (Batch selectors) element
        |> List.concatMap (findAll next)


matchingDescendants : HtmlSelectorGroup msg -> HtmlElement msg -> List (HtmlElement msg)
matchingDescendants selector element =
    Html_.childElements element
        |> List.concatMap (findAll selector)


matchesAll : List (HtmlSelector msg) -> HtmlElement msg -> Bool
matchesAll selectors element =
    List.foldl (\sel result -> 
        case result of
            True ->
                sel element
            False ->
                False
    ) True selectors


queryErrorMessage : Selection msg -> String
queryErrorMessage selection =
  elementToString selection.element
    |> Errors.elementNotFound
    |> Errors.print


elementToString : Maybe (HtmlElement msg) -> String
elementToString maybeElement =
  case maybeElement of
    Just element ->
      ElementPrinter.toString element
    Nothing ->
      "<No Elements>"
