module Elmer.Html.Query exposing
  ( findElement
  , takeElements
  , targetedElement
  , targetedElements
  , findAll
  )

import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as Internal
import Elmer.Component as Component exposing (Component)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Json
import Regex exposing (Regex)


targetedElement : Component model msg -> Maybe (HtmlElement msg)
targetedElement component =
  case component.targetSelector of
    Just selector ->
      findElement selector <| Component.render component
    Nothing ->
      Nothing

targetedElements : Component model msg -> Maybe (List (HtmlElement msg))
targetedElements component =
  case component.targetSelector of
    Just selector ->
      findElements selector <| Component.render component
    Nothing ->
      Nothing

findElement : String -> Html msg -> Maybe (HtmlElement msg)
findElement selector html =
  Native.Html.asHtmlElement html
    |> Maybe.andThen (\rootElement ->
        findAll selector rootElement
          |> List.head
    )

findElements : String -> Html msg -> Maybe (List (HtmlElement msg))
findElements selector html =
  Native.Html.asHtmlElement html
    |> Maybe.map (findAll selector)

findAll : String -> HtmlElement msg -> List (HtmlElement msg)
findAll selector element =
  let
    subselectors = String.split " " selector
  in
    List.foldl (\selector elements ->
      List.concatMap (findAllWithinElement selector) elements
    ) [ element ] subselectors

findAllWithinElement : String -> HtmlElement msg -> List (HtmlElement msg)
findAllWithinElement selector element =
    if matchesElement selector element then
      element ::
        List.concatMap (findAllWithinElement selector) (takeElements element.children)
    else
      List.concatMap (findAllWithinElement selector) (takeElements element.children)


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
                    matchesTagSelector (tagSelector selector) node

        Nothing ->
            False


matchesTagSelector : TagSelector -> HtmlElement msg -> Bool
matchesTagSelector tagSelector node =
    case tagSelector.tag of
        Just tagName ->
            if matchesTag tagName node then
                Maybe.withDefault True <|
                    matchCharacteristicSelector tagSelector node
            else
                False

        Nothing ->
            Maybe.withDefault False <|
                matchCharacteristicSelector tagSelector node


matchCharacteristicSelector : TagSelector -> HtmlElement msg -> Maybe Bool
matchCharacteristicSelector tagSelector node =
    Maybe.map
        (\charName -> matchesCharacteristic charName tagSelector.characteristicValue node)
        tagSelector.characteristicName


type alias TagSelector =
    { tag : Maybe String
    , characteristicName : Maybe String
    , characteristicValue : Maybe String
    }


emptyTagSelector : TagSelector
emptyTagSelector =
    { tag = Nothing
    , characteristicName = Nothing
    , characteristicValue = Nothing
    }


tagSelector : String -> TagSelector
tagSelector selector =
    let
        matchMaybe =
            List.head <|
                Regex.find (Regex.AtMost 1)
                    (Regex.regex "^([\\w-]*)(?:\\[([\\w-]+)(?:='([\\w-]+)')?\\])?")
                    selector
    in
        case matchMaybe of
            Just match ->
                { tag = submatch 0 match
                , characteristicName = submatch 1 match
                , characteristicValue = submatch 2 match
                }

            Nothing ->
                emptyTagSelector


submatch : Int -> Regex.Match -> Maybe String
submatch index match =
    notEmpty << flatten << List.head << List.drop index <| match.submatches

flatten : Maybe ( Maybe a ) -> Maybe a
flatten outerMaybe =
  case outerMaybe of
    Just innerMaybe ->
      innerMaybe
    Nothing ->
      Nothing


notEmpty : Maybe String -> Maybe String
notEmpty maybeEmpty =
    maybeEmpty
        |> Maybe.andThen
            (\s ->
                if String.isEmpty s then
                    Nothing
                else
                    Just s
            )


matchesId : String -> HtmlElement msg -> Bool
matchesId selector node =
    Maybe.withDefault False (Maybe.map ((==) selector) (Internal.elementId node))


matchesClass : String -> HtmlElement msg -> Bool
matchesClass selector node =
    List.member selector (Internal.classList node)


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
  Dict.union (Internal.attributes node) (Internal.properties node)


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
