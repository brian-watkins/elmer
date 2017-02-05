# Elmer

Describe the behavior of Elm HTML applications.

### Install

Elmer requires Elm 0.18.

Right now, Elmer can only be installed manually. First, install [elm-ops-tooling](https://github.com/NoRedInk/elm-ops-tooling); you'll use this to install Elmer. Next, you'll want to set up your project with [elm-test](https://github.com/elm-community/elm-test). Since you'll be deploying Elmer manually, you'll have to make sure its dependencies are available in your tests. So, check the `dependencies` section of `elm-package.json` in your tests directory to be sure that it contains the following:

```
  "elm-lang/html": "2.0.0 <= v < 3.0.0",
  "elm-lang/http": "1.0.0 <= v < 2.0.0",
  "elm-lang/navigation": "2.0.1 <= v < 3.0.0"
```

You'll probably need these in any case if you're building an elm html application that does much at all.

Next, create a test and run it once to make sure that elm-test has downloaded its dependencies. Clone the Elmer repo into some local directory. Finally, run
the following command to install Elmer:

```
$ python ./elm-ops-tooling/elm_self_publish.py <path to elmer> <path to your project>/tests
```

### Usage

#### Create a `ComponentStateResult`

Elmer functions generally pass around `ComponentStateResult` records. To get started describing some
behavior with Elmer, you'll need to generate an initial component state with the `Elmer.componentState`
function. Just pass it your model, view method, and update method.

#### Finding a node

Use `Elmer.Html.find` to find an `HtmlNode` record, which describes a HTML tag in your view. The `find`
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

+ Click events: `Elmer.Html.Event.click <componentStateResult>`
+ Input events: `Elmer.Html.Event.input <text> <componentStateResult>`
+ Custom events: `Elmer.Html.Event.on <eventName> <eventJson> <componentStateResult>`
+ More to come ...

#### Node Matchers

You can make expectations about the targeted node using the `expectNode` function, which takes a
function that maps an HtmlNode to an Expectation and a `ComponentStateResult`. The following
matchers can be used to make expectations about an HtmlNode:

+ `hasId <string> <HtmlNode>`
+ `hasClass <string> <HtmlNode>`
+ `hasText <string> <HtmlNode>`
+ `hasProperty (<string>, <string>) <HtmlNode>`
+ More to come ...

You can also expect that the targeted node exists using the `expectNodeExists` function. You can combine
multiple matchers using the `<&&>` operator like so:

```
Elmer.Html.expectNode (
  Matchers.hasText "Text one" <&&>
  Matchers.hasText "Text two"
) componentStateResult
```

Find the children of a node and make expectations about them like so:

```
Elmer.Html.expectNode (\node ->
  Elmer.Html.Node.findChildren "li" node
    |> List.length
    |> Expect.equal 3
) componentStateResult
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
          Elmer.Html.find "#clickCount" initialState
            |> Elmer.Html.expectNode (
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
      Elmer.Html.find ".button" initialState
        |> Event.click
        |> Event.click
        |> Elmer.Html.find "#clickCount"
        |> Elmer.Html.expectNode (
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

Suppose there is a function `f : a -> b -> Cmd msg` that takes two arguments and produces a command. In order
to specify the effect of this command in our tests, we will *override* occurrences of `f`
with a function we create in our tests. This function will generate a special command
that specifies the intended effect, and Elmer will process the result as if the original command were actually performed.

For a more concrete example, let's test drive a simple app that displays the time.

We'd like to be able to click a button and then see the current time display. We'll begin with a test that describes
the behavior we want:

```
timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        initialState = Elmer.componentState TimeApp.defaultModel TimeApp.view TimeApp.update
      in
        Elmer.Html.find ".button" initialState
          |> Event.click
          |> Elmer.Html.find "#currentTime"
          |> Elmer.Html.expectNode (Matchers.hasText "Time: ???")
  ]
```

To make this compile and pass, we create the following app:

```
type alias Model =
  { }

type Msg
  = Msg

defaultModel : Model
defaultModel =
  { }

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.div [ Attr.id "currentTime" ] [ Html.text ( "Time: ???" ) ]
    , Html.div [ Attr.class "button" ] [ Html.text "Click me for the time!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )
```

Of course, this doesn't really do what we want yet. When the button is clicked,
we'll need our app to actually perform a task, provided by the `Time` module,
that fetches the current time and wraps it with a message, like so:

```
Task.perform NewTime Time.now
```

We want to improve our test so that it drives out this implementation. To do so, we'll need to provide a fake version of `Task.perform`, since Elmer doesn't actually know how to process tasks.

We first create a function in our test with the same signature as `Task.perform`.

```
fakeTaskPerform : Time -> (Time -> msg) -> Task Never Time -> Cmd msg
fakeTaskPerform time tagger _ =
  Command.stub (tagger time)
```

We use `Elmer.Command.stub` to create a command whose effect is the message we provide.
Here, we use it to let us specify exactly what time should be returned for the purpose of our test.

Now we update our test to override `Task.perform` with `fakeTaskPerform`. We use
two functions to do this:

- `Elmer.Command.override` allows us to specify which `Cmd`-generating function to
override and provide the alternate implementation. This produces a `CommandOverride`.
- `Elmer.Command.use` takes a list of `CommandOverride` and a function that operates on a
`ComponentStateResult`. It ensures that the `Cmd`-generating functions will be replaced
with the alternate implementation during the execution of the provided function.

```
timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        initialState = Elmer.componentState TimeApp.defaultModel TimeApp.view TimeApp.update
        taskPerformOverride = Elmer.Command.override (\_ -> Task.perform) (fakeTaskPerform (3 * Time.second))
      in
        Elmer.Html.find ".button" initialState
          |> Elmer.Command.use [ taskPerformOverride ] Event.click
          |> Elmer.Html.find "#currentTime"
          |> Elmer.Html.expectNode (Matchers.hasText "Time: 3000")
  ]
```

Our test should compile but still fail. To make it pass, we'll need to update the rest of our app
so that it requests the current time, stores it in the model, and displays it in the view.

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
    [ Html.div [ Attr.id "currentTime" ] [ Html.text ( "Time: " ++ ( toString model.time ) ) ]
    , Html.div [ Attr.class "button", onClick GetTime ] [ Html.text "Click me for the time!" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetTime ->
      ( model, Task.perform NewTime Time.now )
    NewTime time ->
      ( { model | time = time }, Cmd.none )

```

And now our test should pass.

Notice that we were able to fake out the time task in our test while still avoiding knowledge of
how our app tags the time values as well as other implementation details.
In this way, Elmer allows us to write tests that describe the behavior of our
system while still allowing us the flexibility to refactor our code.  

For the full example, see `tests/Elmer/TestApps/TimeTestApp.elm` and the associated tests in `tests/Elmer/DemoAppTests.elm`.

Note that while Elmer is not capable of processing any commands, it does support
the general operations on commands in the core `Platform.Cmd`, namely, `batch` and `map`. So, you
can use these functions as expected in your components and Elmer should do the right thing.

Elmer provides additional support for HTTP request commands and navigation commands.

#### Elmer.Http

Modern web apps often need to make HTTP requests to some backend server. Elmer makes it easy to stub HTTP
responses and write expectations about the requests made. The `Elmer.Http.Stub` module contains methods
for constructing an `HttpResponseStub` record that describes how to respond to some request. For example:

```
let
  stubbedResponse = HttpStub.get "http://fake.com/search"
    |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
in
```

In this case, a GET request to the given route will result in a response with the given body.
See `Elmer.Http.Stub` for the full list of builder functions. (With more on the way ...)

Once an `HttpResponseStub` has been created, you can use the `Elmer.Http.serve` function
along with `Elmer.Command.use` to override `Http.send` from [elm-lang/http](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/) during a portion of your test.
When your application code calls `Http.send`, the request will be checked against the
provided stubs and if a match occurs, the given response will be returned.

Elmer also allows you to write tests that expect some HTTP request to have been made, in a
manner similar to how you can write expectations about some node in an HTML document. For
example, this test inputs search terms into a field, clicks a search button, and then expects
that a request is made to a specific route with the search terms in the query string:

```
initialComponentState
  |> Elmer.Command.use [ Elmer.Http.serve stubbedResponse ] (
    Elmer.Html.find "input[name='query']"
      >> Elmer.Html.Event.input "Fun Stuff"
      >> Elmer.Html.find "#search-button"
      >> Elmer.Html.Event.click
  )
  |> Elmer.Http.expectGET "http://fake.com/search" (
    Elmer.Http.Matchers.hasQueryParam ("q", "Fun Stuff")
  )
```

If you don't care to describe the behavior of your app after the response from a request is
received -- that is, if you don't care to create a stubbed response for some request -- you
can provide `Elmer.Http.spy` to `Elmer.Command.use` and it will override the `Http.send`
function, merely recording any requests it receives.

See `Elmer.Http` and `Elmer.Http.Matchers` for more.

#### Elmer.Navigation

Elmer provides support for functions in the [elm-lang/navigation](http://package.elm-lang.org/packages/elm-lang/navigation/2.0.1/)
module that allow you to handle navigation for single-page web applications.

To simulate location updates, you must construct a `ComponentStateResult` using
`Elmer.navigationComponentState`. This function is just like `Elmer.componentState`
except that it also takes the location parser function (`Navigation.Location -> msg`)
that you provide to `Navigation.program` when you initialize your app. This provides
Elmer with the information it needs to process location updates as they occur in a test.

You can send a command to update the location manually with the `Elmer.Navigation.setLocation` function.
If your component produces commands to update the location using `Navigation.newUrl` or
`Navigation.modifyUrl`, your tests you should provide `Elmer.Command.use` with `Elmer.Navigation.spy`
so that Elmer will be able to record and process location updates.

You can write an expectation about the current location with `Elmer.Navigation.expectLocation`.

See `tests/Elmer/TestApps/NavigationTestApp.elm` and `tests/Elmer/NavigationTests.elm` for
examples.

#### Sending Arbitrary Commands

Sometimes, a component may be sent a command, either from its parent or as part of initialization.
You can use the `Elmer.Command.send` function to simulate this.

#### Deferred Command Processing

It's often necessary to test the state of a component while some command is running. For example,
one might want to show a progress indicator while an HTTP request is in process. Elmer provides
general support for deferred commands. Use `Elmer.Command.defer` to create a command that
will not be processed until `Elmer.Command.resolveDeferred` is called. Note that all currently
deferred commands will be resolved when this function is called.

`Elmer.Http` allows you to specify when the processing of a stubbed response should be deferred.
When you create your `HttpResponseStub` just use the `Elmer.Http.Stub.deferResponse` builder function
to indicate that this response should be deferred until `Elmer.Command.resolveDeferred` is called.

#### Dummy Commands

You might want to write a test that expects a command to be sent, but doesn't care to describe the
behavior that results from processing that command -- perhaps that is tested somewhere else. In
that case, you could use `Elmer.Command.dummy <identifier>` to create a dummy command.
When Elmer processes a dummy command, it simply records the fact that the command was sent; otherwise
it treats the command just like `Cmd.none`. In your test, use `Elmer.command.expectDummy <identifier>`
to expect that the command was sent.

### Development

To run the tests:

```
$ elm test
```
