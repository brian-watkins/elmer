module Elmer.TestHelpers exposing (..)

import Expect exposing (Expectation)
import Elmer.Html.Types exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Elmer.Html.Node as Node
import Elmer.Html.Printer as HtmlPrinter
import Elmer.Errors as Errors exposing (CustomError)
import Json.Encode as Encode
import Json.Decode as Json


expectError : CustomError -> Expectation -> Expectation
expectError expected actual =
  Expect.equal (Errors.failWith expected) actual


printHtml : Html msg -> String
printHtml html =
  case Node.from html of
    Element element ->
      HtmlPrinter.toString element
    Text text ->
      "<No Elements>"


toElement : Html msg -> HtmlElement msg
toElement html =
  case
    html
      |> Node.from
      |> Node.asElement
  of
    Just element ->
      element
    Nothing ->
      Debug.todo "Could not parse html!"


emptyNode : String -> HtmlElement msg
emptyNode tagName =
  Html.node tagName [] []
    |> toElement


nodeWithTexts : List String -> HtmlElement msg
nodeWithTexts texts =
  List.map Html.text texts
    |> Html.div []
    |> toElement


nodeWithAttributes : List (Html.Attribute msg) -> HtmlElement msg 
nodeWithAttributes attrs =
  Html.div attrs []
    |> toElement


nodeWithClass : String -> HtmlElement msg
nodeWithClass className =
  nodeWithAttributes
  [ Attr.class className
  , Attr.class "funClass"
  ]


nodeWithId : String -> HtmlElement msg
nodeWithId id =
  nodeWithAttributes
  [ Attr.id id
  ]


nodeWithClassAndId : String -> String -> HtmlElement msg
nodeWithClassAndId className id =
  nodeWithAttributes
  [ Attr.id id
  , Attr.class className
  , Attr.class "funClass"
  ]


nodeWithText : String -> HtmlElement msg
nodeWithText text =
  Html.div []
  [ Html.text text 
  ]
    |> toElement


nodeWithList : HtmlElement msg
nodeWithList =
  Html.ul []
  [ Html.li [] []
  , Html.li [] []
  , Html.li [] []
  ]
    |> toElement


nodeWithMultipleChildren : String -> HtmlElement msg
nodeWithMultipleChildren text =
  Html.div []
  [ Html.text "fun stuff"
  , Html.div [] []
  , Html.text text
  ]
    |> toElement


nodeWithNestedChildren : String -> HtmlElement msg
nodeWithNestedChildren text =
  Html.div []
  [ Html.text "fun stuff"
  , Html.div [] []
  , Html.text "another sibling"
  , Html.div []
    [ Html.text text
    ]
  ]
    |> toElement


nodeWithProperty : (String, String) -> HtmlElement msg
nodeWithProperty (name, value) =
  Html.div
  [ Attr.property name <| Encode.string value 
  ] []
    |> toElement


nodeWithBooleanProperty : (String, Bool) -> HtmlElement msg
nodeWithBooleanProperty (name, value) =
  Html.div
  [ Attr.property name <| Encode.bool value 
  ] []
    |> toElement


nodeWithEvents : List String -> HtmlElement String
nodeWithEvents events =
  Html.div
  ( List.map toEventHandler events)
  []
    |> toElement

toEventHandler : String -> Html.Attribute String
toEventHandler event =
  Events.on event <| Json.succeed "fakeEvent"
