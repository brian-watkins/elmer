module Elmer.Html.Selector exposing 
  ( id
  , tag
  , class
  , characteristic
  , text
  , descendantsOf
  , childrenOf
  , by
  )

{-| Functions for selecting Html elements to be targeted during a test.

# Basic Selectors
@docs id, tag, class, characteristic, text

# Group Selectors
@docs by, descendantsOf, childrenOf

-}

import Elmer
import Elmer.Html
import Elmer.Html.Types as Types
import Elmer.Html.Internal as Internal
import Dict exposing (Dict)


{-| Select Html elements by id.
-}
id : String -> Elmer.Html.HtmlSelector msg
id expectedId element =
  Internal.elementId element
    |> Maybe.map ((==) expectedId)
    |> Maybe.withDefault False


{-| Select Html elements by tag name.
-}
tag : String -> Elmer.Html.HtmlSelector msg
tag expectedTag element =
  expectedTag == element.tag


{-| Select Html elements by css class. 
-}
class : String -> Elmer.Html.HtmlSelector msg
class expectedClass element =
  List.member expectedClass (Internal.classList element)


{-| Select Html elements by attribute or property.

Select elements by attribute or property name and value like so:

    testState
      |> Elmer.Html.target 
        << by [ characteristic ( "data-some-attribute", "some-value" ) ]

Specify `Nothing` for the value to select elements that have the named attribute
or property with *any* value.

    testState
      |> Elmer.Html.target 
        << by [ characteristic ( "data-some-attribute", Nothing ) ]

On the difference between attributes and properties,
see [this](https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md).

-}
characteristic : (String, Maybe String) -> Elmer.Html.HtmlSelector msg
characteristic (expectedName, maybeExpectedValue) element =
  let
    characteristics = 
        Dict.union (Internal.attributes element) (Internal.properties element)
  in
    case maybeExpectedValue of
      Just value ->
        Dict.get expectedName characteristics
          |> Maybe.map ((==) value)
          |> Maybe.withDefault False
      Nothing ->
        Dict.member expectedName characteristics


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
text expectedText element =
  Internal.texts element
    |> List.member expectedText


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
      |> Elmer.Html.target << by [ tag "div", class "some-class" ]

-}
by : List (Elmer.Html.HtmlSelector msg) -> targetable -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
by selectors targetable =
  ( Types.ElementWith selectors, targetable )
