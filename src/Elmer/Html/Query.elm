module Elmer.Html.Query exposing
  ( HtmlTarget
  , forHtml
  , forElement
  , forContext
  , findElement
  , findElements
  )

{-| Query Html (exposed for testing only)

@docs HtmlTarget, forHtml, forElement, forContext, findElement, findElements

-}

import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as Html_
import Elmer.Html.Node as Node
import Elmer.Html.TagSelector as TagSelector exposing (TagSelector)
import Elmer.Context as Context exposing (Context)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Json


{-| HtmlTarget
-}
type HtmlTarget msg =
  HtmlTarget (Selection msg)

type alias Selection msg =
  { selector : String
  , element : Maybe (HtmlElement msg)
  }


{-| forHtml
-}
forHtml : String -> Html msg -> HtmlTarget msg
forHtml selector html =
  HtmlTarget
    { selector = selector
    , element =
        Node.from html
          |> Node.asElement
    }


{-| forElement
-}
forElement : String -> HtmlElement msg -> HtmlTarget msg
forElement selector element =
  HtmlTarget
    { selector = selector
    , element = Just element
    }


{-| forContext
-}
forContext : Context model msg -> Maybe (HtmlTarget msg)
forContext context =
  Maybe.map2 (\selector view -> forHtml selector view)
    (Context.state TargetSelector context)
    (Context.render context)


{-| findElement
-}
findElement : HtmlTarget msg -> Result String (HtmlElement msg)
findElement query =
  let
    (HtmlTarget selection) = query
  in
    selection.element
      |> Maybe.andThen (\rootElement ->
          findAll selection.selector rootElement
            |> List.head
        )
      |> Result.fromMaybe (queryErrorMessage query)


{-| findElements
-}
findElements : HtmlTarget msg -> List (HtmlElement msg)
findElements (HtmlTarget selection) =
  selection.element
    |> Maybe.map (findAll selection.selector)
    |> Maybe.withDefault []


findAll : String -> HtmlElement msg -> List (HtmlElement msg)
findAll selector element =
  let
    subselectors = String.split " " selector
  in
    List.foldl (\subSelector elements ->
      List.concatMap (findAllWithinElement subSelector) elements
    ) [ element ] subselectors

findAllWithinElement : String -> HtmlElement msg -> List (HtmlElement msg)
findAllWithinElement selector element =
    if matchesElement selector element then
      element ::
        List.concatMap (findAllWithinElement selector) (takeElements element.children)
    else
      List.concatMap (findAllWithinElement selector) (takeElements element.children)


-- NOTE: This could probably be revisited to use the TagSelector parser.
-- Then it's just a matter of whether it has an id, just a style, or a tag?
-- Also probably would be better to have functions for constructing the selector anyway.
-- But lots of possibilities here ... 
-- Markup.target (byId "blah") (and id can't be combined with style, etc. but attribute-name can be)
-- Markup.target (byTag "div" <| withAttribute ("data-attribute", "value") <| withClass "blah")
-- Markup.target (byAttributeName "data-attribute")
-- Markup.target (byAttributeName "data-attrbute" <| withValue <| withClass)
-- Markup.target (byClass "blah")
-- Markup.target ... with descendants ... 
matchesElement : String -> HtmlElement msg -> Bool
matchesElement selector node =
    case String.uncons selector of
        Just ( selectorType, name ) ->
            case selectorType of
                '#' ->
                    matchesId name node

                '.' ->
                    matchesClass name node

                _ ->
                    matchesTagSelector (TagSelector.from selector) node

        Nothing ->
            False


matchesTagSelector : TagSelector -> HtmlElement msg -> Bool
matchesTagSelector selector node =
    case selector.tag of
        Just tagName ->
            if matchesTag tagName node then
              (matchCharacteristicSelector True selector node)
                && (matchTagClass True selector node)
            else
                False

        Nothing ->
          (matchCharacteristicSelector False selector node)
            && (matchTagClass True selector node)


matchCharacteristicSelector : Bool -> TagSelector -> HtmlElement msg -> Bool
matchCharacteristicSelector isOptional selector node =
    Maybe.map (\charName ->
      matchesCharacteristic charName selector.characteristicValue node
    ) selector.characteristicName
    |> Maybe.withDefault isOptional


matchTagClass : Bool -> TagSelector -> HtmlElement msg -> Bool
matchTagClass isOptional selector element =
  Maybe.map (\className ->
    matchesClass className element
  ) selector.class
  |> Maybe.withDefault isOptional


matchesId : String -> HtmlElement msg -> Bool
matchesId selector node =
    Maybe.withDefault False (Maybe.map ((==) selector) (Html_.elementId node))


matchesClass : String -> HtmlElement msg -> Bool
matchesClass selector node =
    List.member selector (Html_.classList node)


matchesTag : String -> HtmlElement msg -> Bool
matchesTag selector node =
    node.tag == selector


matchesCharacteristic : String -> Maybe String -> HtmlElement msg -> Bool
matchesCharacteristic charName maybeCharValue node =
    let
        characteristics = allCharacteristics node
    in
        Maybe.withDefault (Dict.member charName characteristics) <|
            Maybe.map
                ((==) (Maybe.withDefault "" (Dict.get charName characteristics)))
                maybeCharValue


allCharacteristics : HtmlElement msg -> Dict String String
allCharacteristics node =
  Dict.union (Html_.attributes node) (Html_.properties node)


takeElements : List (HtmlNode msg) -> List (HtmlElement msg)
takeElements =
    List.filterMap
        (\e ->
            case e of
                Element n ->
                    Just n

                _ ->
                    Nothing
        )


queryErrorMessage : HtmlTarget msg -> String
queryErrorMessage (HtmlTarget selection) =
  "No html element found with selector: "
    ++ selection.selector
    ++ "\n\nThe current view is:\n\n"
    ++ (elementToString selection.element)


elementToString : Maybe (HtmlElement msg) -> String
elementToString maybeElement =
  case maybeElement of
    Just element ->
      Html_.toString element
    Nothing ->
      "<No Elements>"
