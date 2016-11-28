module Elmer.Matchers exposing
  ( hasText
  , hasClass
  , hasProperty
  , hasId
  )

import Elmer.Types exposing (..)
import Elmer.Node as Node
import Expect
import String
import Json.Decode as Json

hasText : String -> HtmlNode msg -> Expect.Expectation
hasText text node =
    let
        texts =
            flattenTexts node.children
    in
        if List.length texts == 0 then
            Expect.fail ("Expected node to have text\n\n\t" ++ text ++ "\n\nbut it has no text")
        else if List.member text texts then
            Expect.pass
        else
            Expect.fail ("Expected node to have text\n\n\t" ++ text ++ "\n\nbut it has\n\n\t" ++ (printList texts))


hasClass : String -> HtmlNode msg -> Expect.Expectation
hasClass className node =
    let
        classList =
            Node.classList node
    in
        if List.length classList > 0 then
            if List.member className classList then
                Expect.pass
            else
                Expect.fail ("Expected node to have class\n\n\t" ++ className ++ "\n\nbut it has\n\n\t" ++ (printList classList))
        else
            Expect.fail ("Expected node to have class\n\n\t" ++ className ++ "\n\nbut it has no classes")


hasProperty : (String, String) -> HtmlNode msg -> Expect.Expectation
hasProperty (name, value) node =
  case Node.property name node of
    Just propertyValue ->
      if value == propertyValue then
        Expect.pass
      else
        Expect.fail ("Expected node to have property\n\n\t" ++ name ++ " = " ++ value ++
          "\n\nbut it has\n\n\t" ++ name ++ " = " ++ propertyValue)
    Nothing ->
      Expect.fail ("Expected node to have property\n\n\t" ++ name ++ " = " ++ value ++
        "\n\nbut it has no property with that name")

hasId : String -> HtmlNode msg -> Expect.Expectation
hasId expectedId node =
  case Node.id node of
    Just nodeId ->
      if nodeId == expectedId then
        Expect.pass
      else
        Expect.fail ("Expected node to have id\n\n\t" ++ expectedId ++ "\n\nbut it has id\n\n\t" ++ nodeId)
    Nothing ->
      Expect.fail ("Expected node to have id\n\n\t" ++ expectedId ++ "\n\nbut it has no id")

-- Private functions

flattenTexts : List (HtmlElement msg) -> List String
flattenTexts children =
    List.concat <|
        List.map
            (\child ->
                case child of
                    Node n ->
                        flattenTexts n.children

                    Text t ->
                        [ t ]
            )
            children


printList : List String -> String
printList list =
    String.join ", " list
