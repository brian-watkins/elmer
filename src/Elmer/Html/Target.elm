module Elmer.Html.Target exposing
  ( forHtml
  , forElement
  , forContext
  )

import Elmer.Context as Context exposing (Context)
import Elmer.Html.Node as Node
import Elmer.Html.Types exposing (HtmlTarget(..), HtmlSelectorGroup, HtmlElement, HtmlState(..))
import Html exposing (Html)


forHtml : HtmlSelectorGroup msg -> Html msg -> HtmlTarget msg
forHtml selector html =
  HtmlTarget
    { selector = selector
    , element =
        Node.from html
          |> Node.asElement
    }


forElement : HtmlSelectorGroup msg -> HtmlElement msg -> HtmlTarget msg
forElement selector element =
  HtmlTarget
    { selector = selector
    , element = Just element
    }


forContext : Context model msg -> Maybe (HtmlTarget msg)
forContext context =
  Maybe.map2 (\selector view -> forHtml selector view)
    (Context.state TargetSelector context)
    (Context.render context)
