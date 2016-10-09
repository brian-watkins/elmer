module Elmer.Types exposing (..)

import Html exposing (Html)

type HtmlElement =
  Node HtmlNode |
  Text String

type alias HtmlNode =
  { tag: String
  , id: Maybe String
  , classes: Maybe ( List String )
  , children: HtmlElementList
  , events: Maybe ( HtmlEvents )
  }

type HtmlElementList =
  HtmlElementList (List HtmlElement)

type alias HtmlEvents =
  { click: Maybe HtmlEvent
  , input: Maybe HtmlEvent
  }

type alias HtmlEvent =
  { eventType: String
  , decoder: RawValue
  }

type RawValue = RawValue

type EventResult msg =
  Message msg |
  EventFailure String

type alias ViewFunction model msg =
  model -> Html msg

type alias UpdateFunction model msg =
  msg -> model -> ( model, Cmd msg )

type alias HtmlComponentState model msg =
  { model: model
  , view: ViewFunction model msg
  , update: UpdateFunction model msg
  , targetNode : Maybe HtmlNode
  }

type ComponentStateResult model msg =
  CurrentState (HtmlComponentState model msg) |
  UpstreamFailure String
