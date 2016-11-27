module Elmer.Matchers exposing
  ( hasText
  , hasClass
  , hasProperty
  )

import Elmer exposing (..)
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
            Elmer.classList node
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
  let
    propertyValueResult = Json.decodeString (Json.field name Json.string) node.facts
  in
    case propertyValueResult of
      Ok propertyValue ->
        if value == propertyValue then
          Expect.pass
        else
          Expect.fail ("Expected node to have property\n\n\t" ++ name ++ " = " ++ value ++
            "\n\nbut it has\n\n\t" ++ name ++ " = " ++ propertyValue)
      Err _ ->
        Expect.fail ("Expected node to have property\n\n\t" ++ name ++ " = " ++ value ++
          "\n\nbut it has no property with that name")


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
