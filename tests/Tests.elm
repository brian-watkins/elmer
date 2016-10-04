module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Html exposing (Html, div, text, input, Attribute, li, ul, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Elmer exposing (SearchResult)
import Task exposing (Task)

type alias Model =
  { name: String
  , activity: String
  , clicks: Int
  , numberFromTask: Int
  , numberTaskError: String
  , numberTaskGenerator: Task String Int
  }

defaultModel : Model
defaultModel =
  { name = "Brian"
  , activity = "reading"
  , clicks = 0
  , numberFromTask = -1
  , numberTaskError = "No error"
  , numberTaskGenerator = (makeNumberTaskThatSucceeds True)
  }

type Msg =
  HandleClick |
  ClickForNumber |
  TaskNumber Int |
  HandleNumberTaskError String |
  HandleInput String |
  HandleOtherInput String

view : Model -> Html Msg
view model =
    div [ id "root", class "content" ]
      [ div [ id "userNameLabel", class "label" ] []
      , div
        [ classList [ ("awesome", True), ("super", True), ("root", True) ] ]
        []
      , div [ class "withText" ]
        [ text "Some Fun Text"
        , div [ class "anotherWithText" ] [ text "my text" ]
        , text "Some more text"
        ]
      , input [ class "nameField", onInput HandleInput ] []
      , div [ class "button", onClick HandleClick ] [ text "Click Me" ]
      , div [ id "clickCount" ] [ text ((toString model.clicks) ++ " clicks!") ]
      , div [ id "numberButton", onClick ClickForNumber ] [ text "Get a number!" ]
      , div [ id "numberOutput" ] [ text ("Clicked and got number: " ++ ((toString model.numberFromTask))) ]
      , div [ id "numberOutputError" ] [ text ("Got error requesting number: " ++ model.numberTaskError)]
      ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.none )
    HandleInput inputString ->
      ( { model | name = inputString }, Cmd.none )
    HandleOtherInput inputString ->
      ( { model | activity = inputString }, Cmd.none )
    ClickForNumber ->
      ( model, Task.perform HandleNumberTaskError TaskNumber model.numberTaskGenerator )
    TaskNumber number ->
      ( { model | numberFromTask = number }, Cmd.none )
    HandleNumberTaskError message ->
      ( { model | numberTaskError = message }, Cmd.none )


makeNumberTaskThatSucceeds : Bool -> Task String Int
makeNumberTaskThatSucceeds shouldSucceed =
  if shouldSucceed then
    Task.succeed 3
  else
    Task.fail "Bad things happened!"

eventView : Model -> Html Msg
eventView model =
  input [ id "nameLabel", classList [ ("nameField", True), ("awesome", True) ], onInput HandleInput ] [ text "Click Me" ]

onlyText : Html Msg
onlyText =
  text "Only Text"

all : Test
all =
  describe "Elmer"
    [ findById
    , findByClass
    , noElementFound
    , hasText
    , hasClassTests
    , clickEvent
    , inputEvent
    , doUpdateTests
    , resolveTests
    , findTests
    , clickTests
    , expectNodeTests
    , appFlowTests
    ]

noElementFound : Test
noElementFound =
  describe "when no element found"
    [ describe "with bad id"
      [ test "it returns a failure mesage" <|
        \() ->
          let
            html = view defaultModel
          in
            Expect.equal ( Elmer.findResult html "#nothing" ) (Elmer.SearchFailure "No html node found with selector: #nothing")
      ]
    , describe "with bad class"
      [ test "it returns nothing" <|
        \() ->
          let
            html = view defaultModel
          in
            Expect.equal ( Elmer.findResult html ".nothing" ) (Elmer.SearchFailure "No html node found with selector: .nothing")
      ]
    , describe "when there is only text"
      [ test "it returns nothing" <|
        \() ->
          let
            html = onlyText
          in
            Expect.equal ( Elmer.findResult html ".anything" ) (Elmer.SearchFailure "No html node found with selector: .anything")
      ]
    ]

findById : Test
findById =
    describe "find by id"
        [ test "it finds the top element by id" <|
            \() ->
                let
                  html = view defaultModel
                in
                  case (Elmer.findResult html "#root") of
                    Elmer.Found a ->
                      Expect.equal a.id (Just "root")
                    Elmer.SearchFailure msg ->
                      Expect.fail msg
        , test "finds a nested element by id" <|
            \() ->
                let
                  html = view defaultModel
                in
                  case (Elmer.findResult html "#userNameLabel") of
                    Elmer.Found a ->
                      Expect.equal a.id (Just "userNameLabel")
                    Elmer.SearchFailure msg ->
                      Expect.fail msg
        ]

findByClass : Test
findByClass =
  let
    html = view defaultModel
  in
  describe "find by class"
    [ describe "when there is one class"
      [ test "it finds the top element by class" <|
        \() ->
          case ( Elmer.findResult html ".content" ) of
            Elmer.Found a ->
              Elmer.hasClass "content" a
            Elmer.SearchFailure msg ->
              Expect.fail msg
      , test "it finds a nested element by class" <|
        \() ->
          case ( Elmer.findResult html ".label" ) of
            Elmer.Found a ->
              Elmer.hasClass "label" a
            Elmer.SearchFailure msg ->
              Expect.fail msg
      ]
    , describe "when there is more than one class"
      [ test "it finds the element" <|
        \() ->
          case ( Elmer.findResult html ".awesome" ) of
            Elmer.Found a ->
              Elmer.hasClass "awesome" a
            Elmer.SearchFailure msg ->
              Expect.fail msg
      ]
    , describe "when the class name is the same as an id"
      [ test "it returns the element with the class name" <|
        \() ->
          case ( Elmer.findResult html ".root" ) of
            Elmer.Found a ->
              Elmer.hasClass "root" a
            Elmer.SearchFailure msg ->
              Expect.fail msg
      ]
    ]

emptyNode : String -> Elmer.HtmlNode
emptyNode tagName =
  { tag = tagName
  , id = Nothing
  , classes = Nothing
  , children = (Elmer.HtmlElementList [])
  , events = Nothing
  }

textNode : String -> Elmer.HtmlElement
textNode text =
  Elmer.Text text

nodeWithText : String -> Elmer.HtmlNode
nodeWithText text =
  let
    node = emptyNode "div"
  in
    { node | children = (Elmer.HtmlElementList [(textNode text)]) }

nodeWithClass : String -> Elmer.HtmlNode
nodeWithClass className =
  let
    node = emptyNode "div"
  in
    { node | classes = Just [className, "funClass"] }

nodeWithMultipleChildren : String -> Elmer.HtmlNode
nodeWithMultipleChildren text =
  let
    node = emptyNode "div"
  in
    { node | children = (Elmer.HtmlElementList [(textNode "fun stuff"), Elmer.Node (emptyNode "div"), (textNode text)]) }


hasText : Test
hasText =
  describe "hasText"
  [ describe "when the element has no text"
    [ test "it fails with the right message" <|
      \() ->
        Elmer.hasText "Some text" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have text 'Some text' but it has no text")
    ]
  , describe "when the element has the wrong text"
    [ test "it fails with the right message" <|
      \() ->
        Elmer.hasText "Some text" (nodeWithText "other text")
          |> Expect.equal (Expect.fail "Expected node to have text 'Some text' but it has: other text")
    ]
  , describe "when the element has the text"
    [ test "it passes" <|
      \() ->
        Elmer.hasText "Some text" (nodeWithText "Some text")
          |> Expect.equal Expect.pass
    ]
  , describe "when the element has multiple text nodes, one of which has the text"
    [ test "it passes" <|
      \() ->
        Elmer.hasText "Some text" (nodeWithMultipleChildren "Some text")
          |> Expect.equal Expect.pass
    ]
    , describe "when the element has multiple text nodes, none of which has the text"
      [ test "it fails with the right message" <|
        \() ->
          Elmer.hasText "Other stuff" (nodeWithMultipleChildren "Some text")
            |> Expect.equal (Expect.fail "Expected node to have text 'Other stuff' but it has: fun stuff, Some text")
      ]
  ]

hasClassTests : Test
hasClassTests =
  describe "hasClass"
  [ describe "when the element has no classes"
    [ test "it fails with the right message" <|
      \() ->
        Elmer.hasClass "myClass" (emptyNode "div")
          |> Expect.equal (Expect.fail "Expected node to have class 'myClass' but it has no classes")
    ]
  , describe "when the element has classes"
    [ describe "when the element does not have the specified class"
      [ test "it returns fails with the right message" <|
        \() ->
          Elmer.hasClass "myClass" (nodeWithClass "anotherClass")
            |> Expect.equal (Expect.fail "Expected node to have class 'myClass' but it has: anotherClass, funClass")
      ]
    , describe "when the element has the specified class"
      [ test "it passes" <|
        \() ->
          Elmer.hasClass "myClass" (nodeWithClass "myClass")
            |> Expect.equal Expect.pass
      ]
    ]
  ]

clickEvent =
  describe "click event"
  [ describe "when there is nothing found"
    [ test "it returns nothing" <|
      \() ->
        let
          html = view defaultModel
        in
          Elmer.findResult html "#nothing"
            |> Elmer.clickResult
            |> Expect.equal (Elmer.EventFailure "No html node found with selector: #nothing")
    ]
  , describe "when there is no click event"
    [ test "it returns nothing" <|
      \() ->
        let
          html = view defaultModel
        in
          Elmer.findResult html ".awesome"
            |> Elmer.clickResult
            |> Expect.equal (Elmer.EventFailure "No click event found")
    ]
  , describe "when there is a click event"
    [ test "it returns the message" <|
      \() ->
        let
          html = view defaultModel
        in
          Elmer.findResult html ".button"
            |> Elmer.clickResult
            |> Expect.equal (Elmer.Message HandleClick)
    ]
  ]

inputEvent =
  let
    html = view defaultModel
  in
    describe "input event"
    [ describe "when there is no element"
      [ test "it returns nothing" <|
        \() ->
          Elmer.findResult html ".nothing"
            |> Elmer.inputResult "fun stuff"
            |> Expect.equal (Elmer.EventFailure "No html node found with selector: .nothing")
      ]
    , describe "when there is no input event"
      [ test "it returns nothing" <|
        \() ->
          Elmer.findResult html ".awesome"
            |> Elmer.inputResult "fun stuff"
            |> Expect.equal (Elmer.EventFailure "No input event found")
      ]
    , describe "when there is an input event"
      [ test "it returns the text tagged with the message" <|
        \() ->
          Elmer.findResult html ".nameField"
            |> Elmer.inputResult "fun stuff"
            |> Expect.equal (Elmer.Message (HandleInput "fun stuff"))
      ]
    ]

doUpdateTests =
  let
    html = view defaultModel
  in
    describe "perform update after event"
    [ describe "when there is an upstream failure"
      [ test "it shows the failure" <|
        \() ->
          Elmer.findResult html ".nothing"
            |> Elmer.inputResult "Cool Dude"
            |> Elmer.doUpdate update defaultModel
            |> Expect.equal (Elmer.UpdateFailure "No html node found with selector: .nothing")
      ]
    , describe "when the update succeeds"
      [ test "it shows the expected effect" <|
        \() ->
          let
            expectedEffect = ( { defaultModel | name = "Cool Dude" }, Cmd.none )
          in
            Elmer.findResult html ".nameField"
              |> Elmer.inputResult "Cool Dude"
              |> Elmer.doUpdate update defaultModel
              |> Expect.equal (Elmer.UpdateEffect expectedEffect)
      ]
    ]

resolveTests =
  let
    html = view defaultModel
  in
    describe "resolve results for expectation"
    [ describe "when there is an upstream failure"
      [ test "it fails" <|
        \() ->
          Elmer.findResult html ".nothing"
            |> Elmer.inputResult "Cool Dude"
            |> Elmer.doUpdate update defaultModel
            |> Elmer.resolve
              (\model ->
                Expect.fail "nothing")
            |> Expect.equal (Expect.fail "No html node found with selector: .nothing")
      ]
    , describe "when there are no failures"
      [ describe "when the expectation function fails"
        [ test "it displays the failure message" <|
            \() ->
              Elmer.findResult html ".nameField"
                |> Elmer.inputResult "Cool Dude"
                |> Elmer.doUpdate update defaultModel
                |> Elmer.resolve
                  (\model ->
                    Expect.fail "nothing" )
                |> Expect.equal (Expect.fail "nothing")
        ]
      , describe "when the expectation function succeeds"
        [ test "it displays the success" <|
          \() ->
            Elmer.findResult html ".nameField"
              |> Elmer.inputResult "Cool Dude"
              |> Elmer.doUpdate update defaultModel
              |> Elmer.resolve
                (\model ->
                  Expect.equal model.name "Cool Dude")
              |> Expect.equal (Expect.pass)
        ]
      ]
    ]

findTests =
  describe "find based on component state"
  [ describe "when there is an upstream failure"
    [ test "it returns the failure" <|
      \() ->
        let
          initialState = Elmer.UpstreamFailure "upstream failure"
        in
          Elmer.find ".button" initialState
            |> Expect.equal initialState
    ]
  , describe "when no element is found"
    [ test "it returns the failure message" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.find ".blah" initialState
            |> Expect.equal (Elmer.UpstreamFailure "No html node found with selector: .blah")
    ]
  , describe "when the element is found"
    [ test "it updates the state with the targetnode" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
          stateResult = Elmer.find ".button" initialState
        in
          case stateResult of
            Elmer.CurrentState state ->
              case state.targetNode of
                Just node ->
                  Expect.equal node.tag "div"
                Nothing ->
                  Expect.fail "No target node!"
            Elmer.UpstreamFailure message ->
              Expect.fail message
    ]
  ]

clickTests =
  describe "click based on component state"
  [ describe "when there is an upstream failure"
    [ test "it passes on the error" <|
      \() ->
        let
          initialState = Elmer.UpstreamFailure "upstream failure"
        in
          Elmer.click initialState
            |> Expect.equal initialState
    ]
  , describe "when there is no target node"
    [ test "it returns an upstream failure" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.click initialState
           |> Expect.equal (Elmer.UpstreamFailure "No target node specified")
    ]
  , describe "when there is a target node"
    [ describe "when the click fails"
      [ test "it returns an upstream failure" <|
        \() ->
          let
            initialState = Elmer.initialComponentState defaultModel view update
          in
            Elmer.find ".awesome" initialState
              |> Elmer.click
              |> Expect.equal (Elmer.UpstreamFailure "No click event found")
      ]
    ]
    , describe "when the click succeeds"
      [ test "it updates the model accordingly" <|
        \() ->
          let
            initialState = Elmer.initialComponentState defaultModel view update
            updatedStateResult = Elmer.find ".button" initialState
                                  |> Elmer.click
          in
            case updatedStateResult of
              Elmer.CurrentState updatedState ->
                Expect.equal updatedState.model.clicks 1
              Elmer.UpstreamFailure msg ->
                Expect.fail msg
      ]
  ]

expectNodeTests =
  describe "expect node"
  [ describe "when there is an upstream failure"
    [ test "it fails with the error message" <|
      \() ->
        let
          initialState = Elmer.UpstreamFailure "upstream failure"
        in
          Elmer.expectNode (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "upstream failure")
    ]
  , describe "when there is no target node"
    [ test "it fails with an error" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.expectNode (
            \node -> Expect.fail "Should not get here"
          ) initialState
            |> Expect.equal (Expect.fail "No target node specified")
    ]
  , describe "when there is a target node"
    [ test "it executes the expectation function" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.find ".awesome" initialState
            |> Elmer.expectNode (
                  \node -> Expect.equal "div" node.tag
                )
            |> Expect.equal Expect.pass
    ]
  ]

appFlowTests =
  describe "app flow"
    [ test "it updates the model as events are processed and passes the expectation" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.find ".button" initialState
            |> Elmer.click
            |> Elmer.click
            |> Elmer.find "#clickCount"
            |> Elmer.expectNode (
              \node ->
                Elmer.hasText "2 clicks!" node
            )
            |> Expect.equal (Expect.pass)
    , test "it updates the model with the result of a command" <|
      \() ->
        let
          initialState = Elmer.initialComponentState defaultModel view update
        in
          Elmer.find "#numberButton" initialState
            |> Elmer.click
            |> Elmer.find "#numberOutput"
            |> Elmer.expectNode (
              \node ->
                Elmer.hasText "Clicked and got number: 3" node
            )
            |> Expect.equal Expect.pass
    , test "it handles a task that fails" <|
      \() ->
        let
          modelWithFailingTask = { defaultModel | numberTaskGenerator = (makeNumberTaskThatSucceeds False) }
          initialState = Elmer.initialComponentState modelWithFailingTask view update
        in
          Elmer.find "#numberButton" initialState
            |> Elmer.click
            |> Elmer.find "#numberOutputError"
            |> Elmer.expectNode (
              \node ->
                Elmer.hasText "Got error requesting number: Bad things happened!" node
            )
            |> Expect.equal Expect.pass
    ]
