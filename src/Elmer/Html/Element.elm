module Elmer.Html.Element exposing
  ( id
  , tag
  , target
  , classList
  , property
  , boolProperty
  , properties
  , attributes
  , toString
  )

{-| Functions for working directly with HtmlElements.

# Element Characteristics
@docs tag, id, classList, property, boolProperty, properties, attributes

# Target Descendants
@docs target

# Debugging
@docs toString

-}

-- REVISIT: We might want to add a function to get the styles here?

import Elmer.Html
import Elmer.Html.Internal as Internal
import Elmer.Html.Query as Query
import Elmer.Html.Printer as HtmlPrinter
import Dict exposing (Dict)


{-| Target descendants of an element.

Use this function in conjunction with `HtmlTarget` matchers like `element` or `elements`
to make expectations about descendants of an element.

    Elmer.given someModel view update
      |> Elmer.Html.target "#some-element"
      |> Elmer.Html.expect (Elmer.Html.Matchers.element <|
        \element ->
          target "div" element
            |> Elmer.Html.Matchers.elements (
              Elmer.hasLength 3
            )

Note that `Elmer.Html.target "#some-element div"` would allow you to write the
same expectation. Use `Element.target` for complex expectations about nested elements.

-}
target : String -> Elmer.Html.HtmlElement msg -> Elmer.Html.HtmlTarget msg
target selector =
  Query.forElement selector


{-| Represent an `HtmlElement` as a String.
-}
toString : Elmer.Html.HtmlElement msg -> String
toString node =
  HtmlPrinter.toString node


{-| Get the tag of the element
-}
tag : Elmer.Html.HtmlElement msg -> String
tag =
  Internal.tag


{-| Get the value of the element's `id` attribute, if it is defined.
-}
id : Elmer.Html.HtmlElement msg -> Maybe String
id =
  Internal.elementId


{-| Get a list of classes applied to this element.
-}
classList : Elmer.Html.HtmlElement msg -> List String
classList =
  Internal.classList


{-| Get the value of a particular property belonging to this
element, if that property is defined.
-}
property : String -> Elmer.Html.HtmlElement msg -> Maybe String
property name =
  Internal.property name


{-| Get the boolean value of a particular property belonging to
this element, if that property is defined.

If the property is defined, but its value is not boolean, then
`Nothing` is returned.
-}
boolProperty : String -> Elmer.Html.HtmlElement msg -> Maybe Bool
boolProperty name element =
  property name element
    |> Maybe.andThen toBool


toBool : String -> Maybe Bool
toBool str =
  case str of
    "true" ->
      Just True
    "false" ->
      Just False
    _ ->
      Nothing


{-| Get this element's properties as a `Dict`.

On the difference between attributes and properties,
see [this](https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md).
-}
properties : Elmer.Html.HtmlElement msg -> Dict String String
properties =
  Internal.properties


{-| Get this element's attributes as a `Dict`. If you define a custom attribute
for an Html element, you can find it with this function.

    Elmer.given someModel view update
      |> target "#some-element"
      |> expect (element <| \element ->
        attributes element
          |> Dict.get "data-attribute"
          |> Expect.notEqual Nothing
      )
-}
attributes : Elmer.Html.HtmlElement msg -> Dict String String
attributes =
  Internal.attributes
