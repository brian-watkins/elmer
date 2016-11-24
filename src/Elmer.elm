module Elmer
    exposing
        ( componentState
        , navigationComponentState
        , find
        , findNode
        , expectNode
        , expectNodeExists
        , id
        , classList
        , customAttributesDict
        , map
        , failureCommand
        , HtmlElement(..)
        , HtmlNode
        , HtmlEvent
        , HtmlComponentState
        , ComponentStateResult(..)
        )

import Html exposing (Html)
import Native.Helpers
import String
import Dict exposing (Dict)
import Json.Decode as Json
import Regex exposing (Regex)
import Expect
import Maybe.Extra as MaybeEx
import Navigation
import Task

type HtmlElement msg
    = Node (HtmlNode msg)
    | Text String


type alias HtmlNode msg =
    { tag : String
    , facts : String
    , children : List (HtmlElement msg)
    , events : List (HtmlEvent msg)
    }


type alias HtmlEvent msg =
    { eventType : String
    , decoder : Json.Decoder msg
    }


type alias ViewFunction model msg =
    model -> Html msg


type alias UpdateFunction model msg =
    msg -> model -> ( model, Cmd msg )


type alias LocationParserFunction msg =
    Navigation.Location -> msg


type alias HtmlComponentState model msg =
    { model : model
    , view : ViewFunction model msg
    , update : UpdateFunction model msg
    , targetNode : Maybe (HtmlNode msg)
    , locationParser : Maybe (LocationParserFunction msg)
    , location : Maybe String
    }


type ComponentStateResult model msg
    = CurrentState (HtmlComponentState model msg)
    | UpstreamFailure String


componentState : model -> ViewFunction model msg -> UpdateFunction model msg -> ComponentStateResult model msg
componentState model view update =
    CurrentState
        { model = model
        , view = view
        , update = update
        , targetNode = Nothing
        , locationParser = Nothing
        , location = Nothing
        }


navigationComponentState :
    model
    -> ViewFunction model msg
    -> UpdateFunction model msg
    -> LocationParserFunction msg
    -> ComponentStateResult model msg
navigationComponentState model view update parser =
    CurrentState
        { model = model
        , view = view
        , update = update
        , targetNode = Nothing
        , locationParser = Just parser
        , location = Nothing
        }


map : (HtmlComponentState model msg -> ComponentStateResult model msg) -> ComponentStateResult model msg -> ComponentStateResult model msg
map mapper componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            mapper componentState

        UpstreamFailure message ->
            UpstreamFailure message


expectNode : (HtmlNode msg -> Expect.Expectation) -> ComponentStateResult model msg -> Expect.Expectation
expectNode expectFunction componentStateResult =
    case componentStateResult of
        CurrentState componentState ->
            case componentState.targetNode of
                Just node ->
                    expectFunction node

                Nothing ->
                    Expect.fail "Node does not exist"

        UpstreamFailure message ->
            Expect.fail message


expectNodeExists : ComponentStateResult model msg -> Expect.Expectation
expectNodeExists componentStateResult =
    expectNode (\_ -> Expect.pass) componentStateResult


find : String -> ComponentStateResult model msg -> ComponentStateResult model msg
find selector =
    map (updateTargetNode selector)


updateTargetNode : String -> HtmlComponentState model msg -> ComponentStateResult model msg
updateTargetNode selector componentState =
    case findNode (componentState.view componentState.model) selector of
        Just node ->
            CurrentState { componentState | targetNode = Just node }

        Nothing ->
            UpstreamFailure ("No html node found with selector: " ++ selector)


findNode : Html msg -> String -> Maybe (HtmlNode msg)
findNode html selector =
    Native.Helpers.asHtmlNode html |> Maybe.andThen (findWithinNode selector)


findWithinNode : String -> HtmlNode msg -> Maybe (HtmlNode msg)
findWithinNode selector root =
    if matchesNode selector root then
        Just root
    else
        List.head <|
            List.filterMap (findWithinNode selector) (takeNodes root.children)


matchesNode : String -> HtmlNode msg -> Bool
matchesNode selector node =
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


matchesTagSelector : TagSelector -> HtmlNode msg -> Bool
matchesTagSelector tagSelector node =
    case tagSelector.tag of
        Just tagName ->
            if matchesTag tagName node then
                Maybe.withDefault True <|
                    matchAttributeSelector tagSelector node
            else
                False

        Nothing ->
            Maybe.withDefault False <|
                matchAttributeSelector tagSelector node


matchAttributeSelector : TagSelector -> HtmlNode msg -> Maybe Bool
matchAttributeSelector tagSelector node =
    Maybe.map
        (\attrName -> matchesAttribute attrName tagSelector.attributeValue node)
        tagSelector.attributeName


type alias TagSelector =
    { tag : Maybe String
    , attributeName : Maybe String
    , attributeValue : Maybe String
    }


emptyTagSelector : TagSelector
emptyTagSelector =
    { tag = Nothing
    , attributeName = Nothing
    , attributeValue = Nothing
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
                , attributeName = submatch 1 match
                , attributeValue = submatch 2 match
                }

            Nothing ->
                emptyTagSelector


submatch : Int -> Regex.Match -> Maybe String
submatch index match =
    notEmpty << MaybeEx.join << List.head << List.drop index <| match.submatches


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


matchesId : String -> HtmlNode msg -> Bool
matchesId selector node =
    Maybe.withDefault False (Maybe.map ((==) selector) (id node))


matchesClass : String -> HtmlNode msg -> Bool
matchesClass selector node =
    List.member selector (classList node)


matchesTag : String -> HtmlNode msg -> Bool
matchesTag selector node =
    node.tag == selector


matchesAttribute : String -> Maybe String -> HtmlNode msg -> Bool
matchesAttribute attributeName maybeAttributeValue node =
    let
        attributesDict =
            customAttributesDict node
    in
        Maybe.withDefault (Dict.member attributeName attributesDict) <|
            Maybe.map
                ((==) (Maybe.withDefault "" (Dict.get attributeName attributesDict)))
                maybeAttributeValue


id : HtmlNode msg -> Maybe String
id node =
    Result.toMaybe (Json.decodeString (Json.field "id" Json.string) node.facts)


classList : HtmlNode msg -> List String
classList node =
    case Json.decodeString (Json.field "className" Json.string) node.facts of
        Ok classes ->
            String.split " " classes

        Err _ ->
            []


customAttributesDict : HtmlNode msg -> Dict String String
customAttributesDict node =
    Result.withDefault Dict.empty <|
        Json.decodeString (Json.field "ATTR" (Json.dict Json.string)) node.facts


takeNodes : List (HtmlElement msg) -> List (HtmlNode msg)
takeNodes =
    List.filterMap
        (\e ->
            case e of
                Node n ->
                    Just n

                _ ->
                    Nothing
        )



type alias TaskFailure =
  { elmerError: String
  }

failureCommand : String -> Cmd msg
failureCommand message =
  Task.attempt errorTagger (Task.fail (taskFailure message))

errorTagger : Result TaskFailure a -> msg
errorTagger result =
  case result of
    Ok err ->
      Debug.crash "Died"
    Err _ ->
      Debug.crash "Died"

taskFailure : String -> TaskFailure
taskFailure msg =
  { elmerError = msg }
