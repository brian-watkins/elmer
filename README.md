# Elmer

Describe the behavior of Elm HTML applications.

### Deployment

Right now, the only way to deploy Elmer is by hand. First, go get the [elm-ops-tooling](https://github.com/NoRedInk/elm-ops-tooling). Next, you'll want to set up your project with [elm-test](https://github.com/elm-community/elm-test). Create a test and run it once to make sure that elm-test has downloaded its dependencies. Then, add Elmer like so:

```
$ python ./elm-ops-tooling/elm_self_publish.py ./elmer ./my-project/test/
```

### Usage

#### Create a `ComponentStateResult`

Elmer functions generally pass around `ComponentStateResult` records. To get started describing some
behavior with Elmer, you'll need to generate an initial component state with the `Elmer.componentState`
function. Just pass it your model, view method, and update method.

#### Finding a node

Use `Elmer.find` to find an `HtmlNode` record, which describes a HTML tag in your view. The `find`
function takes a selector and a `ComponentStateResult` as arguments. The selector can take the following
formats:

+ To find the first node with the class myClass, use `.myClass`
+ To find a node with the id myId, use `#myId`
+ To find the first div tag, use `div`
+ To find the first div tag with the custom data attribute data-node, use `div[data-node]`
+ To find the first div tag with the attribute data-node and the value myData, use `div[data-node='myData']`

#### Taking action on a node

Once you find a node, that node is _targeted_ as the subject of subsequent actions, that is, until
you find another node. The following functions define actions on nodes:

+ Click events: Elmer.Event.click <componentStateResult>
+ Input events: Elmer.Event.input <text> <componentStateResult>
+ Custom events: Elmer.Event.on <eventName> <eventJson> <componentStateResult>

#### Node Matchers

You can make expectations about the targeted node using the `expectNode` function, which takes a
function that maps an HtmlNode to an Expectation. The following matchers can be used to make
expectations about an HtmlNode:

+ hasText <string> <HtmlNode>
+ hasClass <string> <HtmlNode>

You can also expect that the targeted node exists using the `expectNodeExists` function.

#### Manually updating a component

Sometimes, a component may respond to a message from its parent or otherwise need to update in response
to some message, say as part of initialization. In such cases, you can use the `Elmer.Event.command` function
to send your component a message that will be fed through the `update` method, producing a new
`componentStateResult`. 

### Example

Let's test-drive a simple Elm HTML Application. We want to have a button on the screen that, when clicked, updates a counter. First, we write a test, using [Elm-Test](https://github.com/elm-community/elm-test) and Elmer:

```
allTests : Test
allTests =
  let
    initialState = Elmer.componentState App.defaultModel App.view App.update
  in
    describe "my app"
    [ describe "initial state"
      [ test "it shows that no clicks have occurred" <|
        \() ->
          Elmer.find "#clickCount" initialState
            |> Elmer.expectNode (
                \node ->
                  Matchers.hasText "0 clicks!" node
            )      
      ]
    ]
```

Our test finds the html element containing the counter text by its id and checks that it has the text we expect when the app first appears. Let's make it pass:

```
type alias Model =
  { clicks: Int }

defaultModel : Model
defaultModel =
  { clicks = 0 }

type Msg =
  NothingYet

view : Model -> Html Msg
view model =
  div [ id "clickCount" ] [ text "0 clicks!") ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model, Cmd.None )
```

If we run our test now, it should pass.

Now, let's add a new test that describes what we expect to happen when a button is clicked.

```
  describe "when the button is clicked"
  [ test "it updates the counter" <|
    \() ->
      Elmer.find ".button" initialState
        |> Event.click
        |> Event.click
        |> Elmer.find "#clickCount"
        |> Elmer.expectNode (
          \node ->
            Matchers.hasText "2 clicks!" node
        )
  ]
```

This should fail, since we don't even have a button in our view yet. Let's fix that. We'll add a button with a click event handler that sends a message we can handle in the update function. We update the `Msg` type, the `view` function, and the `update` function like so:

```
type Msg =
  HandleClick

view : Model -> Html Msg
view model =
  div []
    [ div [ id "clickCount" ] [ text ((toString model.clicks) ++ " clicks!") ]
    , div [ class "button", onClick HandleClick ] [ text "Click Me" ]  
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HandleClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.None )
```

And our test should pass.

Notice that we were able to test-drive our app, writing our tests first, without worrying about implementation details like the names of the messages our `update` function will use and so on. Elmer simulates the elm architecture workflow, delivering messages to the `update` function when events occur and passing the result to the `view` function so you can write expectations about the updated html. Elmer exposes an `HtmlNode` record and matchers that make it easy to write expectations about the html generated by the `view` function.

Elmer makes it easy to practice behavior-driven development with Elm.

### Commands

Often the `update` function will result in a command to perform some `Task`. Elmer provides support for executing simple tasks generated by the `Task.succeed` or `Task.fail` functions. Of course, your app will probably need to execute more complicated tasks, such as HTTP requests. To make your app easy to test, you'll want to structure your model such that it contains the functions that generate such tasks. Then, in your test, you can provide a model that replaces these functions with either `Task.succeed` or `Task.fail` to describe the behavior for each case.

Elmer also has support for recording navigation commands issued via the [elm-navigation](http://package.elm-lang.org/packages/elm-lang/navigation/1.0.0/Navigation) package. As part of a test, your app can issue such commands (e.g. `Navigation.newUrl` and `Navigation.modifyUrl`) like normal. Then use the `Elmer.Browser.expectLocation` matcher to describe the behavior you expect.

### Development

To run the tests:

```
$ elm test
```
