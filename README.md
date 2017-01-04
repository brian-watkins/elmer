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

+ Click events: `Elmer.Event.click <componentStateResult>`
+ Input events: `Elmer.Event.input <text> <componentStateResult>`
+ Custom events: `Elmer.Event.on <eventName> <eventJson> <componentStateResult>`
+ Sending a command: `Elmer.Event.sendCommand <Cmd> <componentStateResult>`
+ More to come ...

#### Node Matchers

You can make expectations about the targeted node using the `expectNode` function, which takes a
function that maps an HtmlNode to an Expectation. The following matchers can be used to make
expectations about an HtmlNode:

+ `hasId <string> <HtmlNode>`
+ `hasClass <string> <HtmlNode>`
+ `hasText <string> <HtmlNode>`
+ `hasProperty (<string>, <string>) <HtmlNode>`
+ More to come ...

You can also expect that the targeted node exists using the `expectNodeExists` function. You can combine
multiple matchers using the `<&&>` operator like so:

```
Elmer.expectNode (
  Matchers.hasText "Text one" <&&>
  Matchers.hasText "Text two"
)
```

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
                  Matchers.hasText "0 clicks!"
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
            Matchers.hasText "2 clicks!"
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

Commands describe actions to be performed by the Elm runtime. Elmer simulates the Elm runtime in order to
facilitate testing, but it is not intended to replicate the Elm runtime's ability to carry out commands.
Instead, Elmer allows you to specify what effect should result from running a command so that you can then
describe the behavior that follows.

#### Faking Effects

Suppose you have a function `f : a -> b -> Cmd msg` that takes two arguments and produces a command. In order
to specify the effect of this command in our tests, we will inject into our component another function
with the same signature: `fakeF : a -> b -> Cmd msg`. This function will generate a special command
that specifies the intended effect, and Elmer will process the result as if the original command were actually performed.

Here's a more concrete example. Let's say that we have a simple app that displays the time:

```
type alias Model =
  { time : Time }

type Msg
  = GetTime
  | NewTime Time

defaultModel : Model
defaultModel =
  { time = Time.second }

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.div [ id "currentTime" ] [ Html.text ("Time: " ++ (toString model.time)) ]
    , Html.div [ class "button", onClick GetTime ] [ Html.text "Click me for the time!" ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetTime ->
      ( model, Task.perform NewTime Time.now )
    NewTime time ->
      ( { model | time = time }, Cmd.none )
```

Elmer doesn't actually know how to process a task that will return the time. But, by injecting
a function that uses `Elmer.Command.messageCommand`, which lets us create a command whose effect is the message we provide, we can specify exactly what time we want for
our tests. First, we create a fake version of `Task.perform` in our test module:

```
fakeTimeTask : Time -> (Time -> Msg) -> Task Never Time -> Cmd Msg
fakeTimeTask time tagger task =
  Command.messageCommand (tagger time)
```

Then we restructure our app so that we can inject this function into the update method:

```
update : Msg -> Model -> ( Model, Cmd Msg )
update =
  updateWithDependencies Task.perform

type alias SendTimeTask =
  (Time -> Msg) -> Task Never Time -> Cmd Msg

updateWithDependencies : SendTimeTask -> Msg -> Model -> ( Model, Cmd Msg )
updateWithDependencies sendTimeTask msg model =
  case msg of
    GetTime ->
      ( model, sendTimeTask NewTime Time.now )
    NewTime time ->
      ( { model | time = time }, Cmd.none )
```

Now, we can write a test that describes the behavior we expect, given that the time task
will return a specific time when processed by Elmer:

```
timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        testUpdate = TimeApp.updateWithDependencies (fakeTimeTask (3 * Time.second))
        initialState = Elmer.componentState TimeApp.defaultModel TimeApp.view testUpdate
      in
        Elmer.find ".button" initialState
          |> Event.click
          |> Elmer.find "#currentTime"
          |> Elmer.expectNode (Matchers.hasText "Time: 3000")
  ]
```

Using this strategy, we can manage the effects that enter our system while it is under test
and thereby describe the behavior of that system as it processes commands.

Notice that we were able to fake out the time task in our test while still avoiding knowledge of the particular messages used to tag the time values. This is important if we want to be able to write tests that describe the behavior of our system while still allowing us the flexibility to refactor our code.  

For the full example, see `tests/Elmer/TestApps/TimeTestApp.elm` and the associated tests in `tests/Elmer/DemoAppTests.elm`.

Elmer provides additional support for HTTP request commands and navigation commands.

#### Elmer.Http

More to come ...

#### Elmer.Navigation

More to come ...

#### Sending Arbitrary Commands

Sometimes, a component may be sent a command, either from its parent or as part of initialization.
You can use the `Elmer.Event.sendCommand` function to simulate this.


### Development

To run the tests:

```
$ elm test
```
