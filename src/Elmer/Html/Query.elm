module Elmer.Html.Query exposing
  ( HtmlTarget
  , forHtml
  , forElement
  , forContext
  , findElement
  , findElements
  )

import Elmer.Html.Types exposing (..)
import Elmer.Html.Internal as Html_
import Elmer.Html.Node as Node
import Elmer.Context as Context exposing (Context)
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Json
import Regex exposing (Regex)

type HtmlTarget msg =
  HtmlTarget (Selection msg)

type alias Selection msg =
  { selector : String
  , element : Maybe (HtmlElement msg)
  }


forHtml : String -> Html msg -> HtmlTarget msg
forHtml selector html =
  HtmlTarget
    { selector = selector
    , element =
        Node.from html
          |> Node.asElement
    }


forElement : String -> HtmlElement msg -> HtmlTarget msg
forElement selector element =
  HtmlTarget
    { selector = selector
    , element = Just element
    }


forContext : Context model msg -> Maybe (HtmlTarget msg)
forContext context =
  Context.state TargetSelector context
    |> Maybe.map (\selector ->
      forHtml selector <| Context.render context
    )


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
