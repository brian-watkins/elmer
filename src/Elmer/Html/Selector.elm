module Elmer.Html.Selector exposing 
  ( id
  , tag
  , class
  , characteristic
  , within
  , by
  )

{-| Functions for selecting Html elements to be targeted during a test.

# Basic Selectors
@docs id, tag, class, characteristic

# Group Selectors
@docs by, within

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


{-| Select Html elements by which to narrow the scope of further selections.

Suppose you want to select all `li` that are descendants of an `ol` that is itself a descendant of
an element with a particular class:

    testState
      |> Elmer.Html.target
        << within [ class "some-class" ] 
        << within [ tag "ol" ] 
        << by [ tag "li" ]

You can chain as many `within` calls as you like, but the chain must be terminated with a call to `by`.

Note that `within` is inclusive. In the above example, if an `ol` tag itself has the class 
`some-class` then all its descending `li` elements will be selected. Likewise, the following:

    testState
      |> Elmer.Html.target
        << within [ class "some-class" ]
        << by [ tag "p" ]

would select

    <p class="some-class">Some text</p>

and the `p` from

    <div class="some-class">
      <p>Some text</p>
    </div>

-}
within : List (Elmer.Html.HtmlSelector msg) -> (Elmer.Html.HtmlSelectorGroup msg, targetable) -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
within selectors (next, targetable) =
  ( Types.Within selectors next, targetable )


{-| Select Html elements that match all the given selectors.

This would select all `div` elements that have the class `some-class`:

    testState
      |> Elmer.Html.target << by [ tag "div", class "some-class" ]

-}
by : List (Elmer.Html.HtmlSelector msg) -> targetable -> (Elmer.Html.HtmlSelectorGroup msg, targetable)
by selectors targetable =
  ( Types.Batch selectors, targetable )
