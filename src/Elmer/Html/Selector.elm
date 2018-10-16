module Elmer.Html.Selector exposing 
  ( id
  , tag
  , class
  , attribute
  , attributeName
  , text
  , descendantsOf
  , childrenOf
  , by
  )

{-| Functions for selecting Html elements to be targeted during a test.

# Basic Selectors
@docs id, tag, class, attribute, attributeName, text

# Group Selectors
@docs by, descendantsOf, childrenOf

-}

import Elmer
import Elmer.Html
import Elmer.Html.Types as Types
import Elmer.Html.Element.Internal as Internal
import Dict exposing (Dict)


{-| Select Html elements by id.
-}
id : String -> Elmer.Html.HtmlSelector msg
id expectedId =
  { description = "id '" ++ expectedId ++ "'"
  , predicate = \element ->
      Internal.elementId element
        |> Maybe.map ((==) expectedId)
        |> Maybe.withDefault False
  }


{-| Select Html elements by tag name.
-}
tag : String -> Elmer.Html.HtmlSelector msg
tag expectedTag =
  { description = "tag '" ++ expectedTag ++ "'"
  , predicate = \element ->
      expectedTag == element.tag
  }


{-| Select Html elements by css class. 
-}
class : String -> Elmer.Html.HtmlSelector msg
class expectedClass =
  { description = "class '" ++ expectedClass ++ "'"
  , predicate = \element ->
      List.member expectedClass (Internal.classList element)
  }


{-| Select Html elements that have an attribute or proprty with the given name, regardless of the associated value.

On the difference between attributes and properties,
see [this](https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md).
-}
attributeName : String -> Elmer.Html.HtmlSelector msg
attributeName expectedName =
  { description = "attributeName '" ++ expectedName ++ "'"
  , predicate = \element ->
      Internal.allAttrs element
        |> Dict.member expectedName
  }


{-| Select Html elements that have an attribute or property with the given name and value.

On the difference between attributes and properties,
see [this](https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md).
-}
attribute : (String, String) -> Elmer.Html.HtmlSelector msg
attribute (expectedName, expectedValue) =
  { description = "attribute '" ++ expectedName ++ "' = '" ++ expectedValue ++ "'"
  , predicate = \element ->
      Internal.allAttrs element
        |> Dict.get expectedName
        |> Maybe.map ((==) expectedValue)
        |> Maybe.withDefault False  
  }


{-| Select Html elements that have the given text as an immediate descendant.

For example,

    testState
      |> target << by [ text "Some text" ]

would select only the `p` element in the following:

    Html.div []
    [ Html.p [] [ Html.text "Some text" ]
    ]

-}
text : String -> Elmer.Html.HtmlSelector msg
text expectedText =
  { description = "text '" ++ expectedText ++ "'"
  , predicate = \element ->
      Internal.texts element
        |> List.member expectedText
  }


{-| Narrow the scope of further selections to descendants of the selected elements.

Suppose you want to select all `li` that are descendants of an `ol` that is itself a descendant of
an element with a particular class:

    testState
      |> Elmer.Html.target
        << descendantsOf [ class "some-class" ]
        << descendantsOf [ tag "ol" ]
        << by [ tag "li" ]

You can chain as many `descendantsOf` calls as you like, but the chain must be terminated with a call to `by`.
-}
descendantsOf : List (Elmer.Html.HtmlSelector msg) -> (Elmer.Html.HtmlSelectorGroup msg, targetable) -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
descendantsOf selectors (next, targetable) =
  ( Types.DescendantsOf selectors next, targetable )


{-| Narrow the scope of further selections to those elements that are children of the selected elements.

Suppose you want to select all `li` that are children of an `ol`:

    testState
      |> Elmer.Html.target
        << childrenOf [ tag "ol" ]
        << by [ tag "li" ]
-}
childrenOf : List (Elmer.Html.HtmlSelector msg) -> (Elmer.Html.HtmlSelectorGroup msg, targetable) -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
childrenOf selectors (next, targetable) =
  ( Types.ChildrenOf selectors next, targetable )


{-| Select Html elements that match all the given selectors.

This would select all `div` elements that have the class `some-class`:

    testState
      |> Elmer.Html.target
          << by [ tag "div", class "some-class" ]
-}
by : List (Elmer.Html.HtmlSelector msg) -> targetable -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
by selectors targetable =
  ( Types.ElementWith selectors, targetable )
