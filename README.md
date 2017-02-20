# Elmer

Elmer makes it easy to describe the behavior of Elm HTML applications.

### Why?

Behavior-driven development is a great practice to follow when writing
applications. If you describe the behavior of your app in code, it's easy to add
new features or refactor the code with confidence: just run the tests to see
if your app still has all the behavior you've described in those tests.

Elm is a really great language for writing web applications. However, practicing BDD
in Elm can be difficult. The two main functions in the Elm architecture -- `view` and
`update` -- return opaque types, which cannot be inspected for testing purposes.
Even so, calling `view` or `update` directly requires knowledge of an application's
implementation: the shape of its model, its messages, and so on. If writing
tests requires knowledge of implementation details, you lose the biggest
benefit of writing tests in the first place: the ability to change your code
with confidence.

Elmer allows you to describe the behavior of your app without knowledge of
implementation details. It simulates the Elm architecture, calling `view`
and `update` as necessary throughout the course of your test. It lets
you manage how commands and subscriptions are processed so you can
describe the behavior of your app under whatever conditions you need. Elmer
allows you to write tests first, which gives you the freedom and confidence
to change your code later on.


### Usage

Elmer requires Elm 0.18.

#### Create a `ComponentState`

Elmer functions generally pass around `ComponentStates`. To get started describing some
behavior with Elmer, you'll need to generate an initial component state with the `Elmer.componentState`
function. Just pass it your model, view method, and update method.

#### Finding a node

Use `Elmer.Html.find` to find an `HtmlElement`, which describes a HTML element in your view. The `find`
function takes a selector and a `ComponentState` as arguments. The selector can take the following
formats:

+ To find the first element with the class myClass, use `.myClass`
+ To find a element with the id myId, use `#myId`
+ To find the first div tag, use `div`
+ To find the first div tag with the custom data attribute data-node, use `div[data-node]`
+ To find the first div tag with the attribute data-node and the value myData, use `div[data-node='myData']`

#### Taking action on an element

Once you find an element, that element is _targeted_ as the subject of subsequent actions, that is, until
you find another element. The following functions define actions on elements:

+ Click events: `Elmer.Html.Event.click <componentState>`
+ Input events: `Elmer.Html.Event.input <text> <componentState>`
+ Custom events: `Elmer.Html.Event.on <eventName> <eventJson> <componentState>`
+ More to come ...

#### Element Matchers

You can make expectations about the targeted element using the `expectElement` function, which takes a
function that maps an HtmlElement to an Expectation and a `ComponentState`. The following
matchers can be used to make expectations about an HtmlElement:

+ `hasId <string> <HtmlElement>`
+ `hasClass <string> <HtmlElement>`
+ `hasText <string> <HtmlElement>`
+ `hasProperty (<string>, <string>) <HtmlElement>`
+ More to come ...

You can also expect that the targeted node exists using the `expectElementExists` function. You can combine
multiple matchers using the `<&&>` operator like so:

```
Elmer.Html.expectElement (
  Matchers.hasText "Text one" <&&>
  Matchers.hasText "Text two"
) componentStateResult
```

Find the children of an element and make expectations about them like so:

```
Elmer.Html.expectElement (\element ->
  Elmer.Html.findChildren "li" element
    |> List.length
    |> Expect.equal 3
) componentState
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
            |> Elmer.Html.expectElement (
                  Elmer.Html.Matchers.hasText "0 clicks!"
            )      
      ]
    ]
```

Our test finds the html element containing the counter text by its id and checks that it has the text we expect when the app first appears. Let's make it pass:

```
import Html as Html
import Html.Attributes as Attr

type alias Model =
  { clicks: Int }

defaultModel : Model
defaultModel =
  { clicks = 0 }

type Msg =
  NothingYet

view : Model -> Html Msg
view model =
  Html.div [ Attr.id "clickCount" ] [ Html.text "0 clicks!" ]

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
        |> Elmer.Html.Event.click
        |> Elmer.Html.Event.click
        |> Elmer.Html.find "#clickCount"
        |> Elmer.Html.expectElement (
            Elmer.Html.Matchers.hasText "2 clicks!"
        )
  ]
```

This should fail, since we don't even have a button in our view yet. Let's fix that. We'll add a button with a click event handler that sends a message we can handle in the update function. We update the `Msg` type, the `view` function, and the `update` function like so:

```
import Html.Events as Events

type Msg =
  HandleClick

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.div [ Attr.id "clickCount" ] [ Html.text ((toString model.clicks) ++ " clicks!") ]
    , Html.div [ Attr.class "button", Events.onClick HandleClick ] [ Html.text "Click Me" ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    HandleClick ->
      ( { model | clicks = model.clicks + 1 }, Cmd.None )
```

And our test should pass.

Notice that we were able to test-drive our app, writing our tests first, without worrying about implementation details like the names of the messages our `update` function will use and so on. Elmer simulates the elm architecture workflow, delivering messages to the `update` function when events occur and passing the result to the `view` function so you can write expectations about the updated html.

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
          |> Elmer.Html.expectElement (Matchers.hasText "Time: ???")
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

Here, we use `Elmer.Platform.Command.stub` to create a command whose effect is the message we provide.
This lets us specify exactly what time should be returned for the purpose of our test.

Now we update our test to override `Task.perform` with `fakeTaskPerform`. We use
two functions to do this:

- `Elmer.Platform.Command.override` allows us to specify which `Cmd`-generating function to
override and provide the alternate implementation.
- `Elmer.Platform.Command.use` ensures that, during the appropriate portion of our test,
`Cmd`-generating functions will be replaced with the alternate implementations we specify.

```
timeAppTests : Test
timeAppTests =
  describe "time demo app"
  [ test "it displays the time" <|
    \() ->
      let
        initialState = Elmer.componentState TimeApp.defaultModel TimeApp.view TimeApp.update
        taskPerformOverride = Elmer.Platform.Command.override (\_ -> Task.perform) (fakeTaskPerform (3 * Time.second))
      in
        Elmer.Html.find ".button" initialState
          |> Elmer.Platform.Command.use [ taskPerformOverride ] Event.click
          |> Elmer.Html.find "#currentTime"
          |> Elmer.Html.expectElement (Matchers.hasText "Time: 3000")
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

Elmer allows us to write tests first, describing the behavior of our component
without needing to know the internal details of that component. We can provide fake
implementations for `Cmd`-generating functions that our component relies upon, but
in doing so, we don't need to know any details about how our component processes
the effects of the generated commands. Our tests give us flexibility
to refactor our code and confidence that the new code still results in
the correct behavior.

For the full example, see `tests/Elmer/TestApps/TimeTestApp.elm` and the associated tests in `tests/Elmer/DemoAppTests.elm`.

Note that while Elmer is not capable of processing any commands, it does support
the general operations on commands in the core `Platform.Cmd` module, namely, `batch` and `map`. So, you
can use these functions as expected in your components and Elmer should do the right thing.

Elmer provides additional support for HTTP request commands and navigation commands.

#### Elmer.Http

Modern web apps often need to make HTTP requests to some backend server. Elmer makes it easy to stub HTTP
responses and write expectations about the requests made. The `Elmer.Http.Stub` module contains methods
for constructing an `HttpResponseStub` that describes how to respond to some request. For example:

```
let
  stubbedResponse = HttpStub.get "http://fake.com/search"
    |> HttpStub.withBody "{\"name\":\"Super Fun Person\",\"type\":\"person\"}"
in
```

In this case, a GET request to the given route will result in a response with the given body.
See `Elmer.Http.Stub` for the full list of builder functions. (With more on the way ...)

Once an `HttpResponseStub` has been created, you can use the `Elmer.Http.serve` function
along with `Elmer.Platform.Command.use` to override `Http.send` from [elm-lang/http](http://package.elm-lang.org/packages/elm-lang/http/1.0.0/) during a portion of your test.
When your application code calls `Http.send`, the request will be checked against the
provided stubs and if a match occurs, the given response will be returned.

Elmer also allows you to write tests that expect some HTTP request to have been made, in a
manner similar to how you can write expectations about some element in an HTML document. For
example, this test inputs search terms into a field, clicks a search button, and then expects
that a request is made to a specific route with the search terms in the query string:

```
initialComponentState
  |> Elmer.Html.find "input[name='query']"
  |> Elmer.Html.Event.input "Fun Stuff"
  |> Elmer.Html.find "#search-button"
  |> Elmer.Platform.Command.use [ Elmer.Http.serve stubbedResponse ] Elmer.Html.Event.click
  |> Elmer.Http.expectGET "http://fake.com/search" (
    Elmer.Http.Matchers.hasQueryParam ("q", "Fun Stuff")
  )
```

If you don't care to describe the behavior of your app after the response from a request is
received -- that is, if you don't care to create a stubbed response for some request -- you
can provide `Elmer.Http.spy` to `Elmer.Platform.Command.use` and it will override the `Http.send`
function, merely recording any requests it receives.

See `Elmer.Http` and `Elmer.Http.Matchers` for more.

#### Elmer.Navigation

Elmer provides support for functions in the [elm-lang/navigation](http://package.elm-lang.org/packages/elm-lang/navigation/2.0.1/)
module that allow you to handle navigation for single-page web applications.

To simulate location updates, you must construct a `ComponentState` using
`Elmer.Navigation.navigationComponentState`. This function is just like `Elmer.componentState`
except that it also takes the location parser function (`Navigation.Location -> msg`)
that you provide to `Navigation.program` when you initialize your app. This provides
Elmer with the information it needs to process location updates as they occur in a test.

You can send a command to update the location manually with the `Elmer.Navigation.setLocation` function.
If your component produces commands to update the location using `Navigation.newUrl` or
`Navigation.modifyUrl`, your tests you should provide `Elmer.Platform.Command.use` with `Elmer.Navigation.spy`
so that Elmer will be able to record and process location updates.

You can write an expectation about the current location with `Elmer.Navigation.expectLocation`.

See `tests/Elmer/TestApps/NavigationTestApp.elm` and `tests/Elmer/NavigationTests.elm` for
examples.

#### Sending Arbitrary Commands

Sometimes, a component may be sent a command, either from its parent or as part of initialization.
You can use the `Elmer.Platform.Command.send` function to simulate this.

#### Deferred Command Processing

It's often necessary to test the state of a component while some command is running. For example,
one might want to show a progress indicator while an HTTP request is in process. Elmer provides
general support for deferred commands. Use `Elmer.Platform.Command.defer` to create a command that
will not be processed until `Elmer.Platform.Command.resolveDeferred` is called. Note that all currently
deferred commands will be resolved when this function is called.

`Elmer.Http` allows you to specify when the processing of a stubbed response should be deferred.
When you create your `HttpResponseStub` just use the `Elmer.Http.Stub.deferResponse` builder function
to indicate that this response should be deferred until `Elmer.Platform.Command.resolveDeferred` is called.

#### Dummy Commands

You might want to write a test that expects a command to be sent, but doesn't care to describe the
behavior that results from processing that command -- perhaps that is tested somewhere else. In
that case, you could use `Elmer.Platform.Command.dummy <identifier>` to create a dummy command.
When Elmer processes a dummy command, it simply records the fact that the command was sent; otherwise
it treats the command just like `Cmd.none`. In your test, use `Elmer.command.expectDummy <identifier>`
to expect that the command was sent.

### Subscriptions

Using subscriptions, your component can register to be notified when certain effects occur.
To describe the behavior of a component that has subscriptions, you'll need to do two things:

1. Override the function that generates the subscription using `Elmer.Platform.Subscription.use` and
`Elmer.Platform.Subscription.override` and replace it with a `Elmer.Platform.Subscription.spy`
2. Simulate the effect you've subscribed to receive with `Subscription.send`

Let's test-drive a component that subscribes to receive the time every second. We'll
begin by writing a test to drive out the basics of our component.

```
timeDefaultTest : Test
timeDefaultTest =
  describe "before the time is received"
  [ test "it prints 0 seconds" <|
    \() ->
      Elmer.componentState App.defaultModel App.view App.update
        |> Elmer.Html.find "#num-seconds"
        |> Elmer.Html.expectElement (Matchers.hasText "0 seconds")
  ]
```

Let's next do the simplest thing to get this test to compile:

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
    [ Html.div [ Attr.id "num-seconds" ] [ Html.text "0 seconds" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions =
  Sub.none
```

Now let's write a test that describes the behavior when a time effect is received.

```
timeSubscriptionTest : Test
timeSubscriptionTest =
  describe "when a time effect is received"
  [ test "it prints the number of seconds" <|
    \() ->
      let
        timeOverride = Elmer.Platform.Subscription.override (\_ -> Time.every) (\_ tagger ->
            Elmer.Platform.Subscription.spy "timeEffect" tagger
          )
      in
        Elmer.componentState App.defaultModel App.view App.update
          |> Elmer.Platform.Subscription.use [ timeOverride ] App.subscriptions
          |> Elmer.Platform.Subscription.send "timeEffect" (3 * 1000)
          |> Elmer.Html.find "#num-seconds"
          |> Elmer.Html.expectElement ( Matchers.hasText "3 seconds" )
  ]
```

Now we have a failing test; let's make it pass!

```
type alias Model =
  { currentTime : Time }

type Msg
  = TimeUpdate Time

defaultModel : Model
defaultModel =
  { currentTime = 0 }

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.div [ Attr.id "num-seconds" ] [ Html.text ((formatTime model.currentTime) ++ " seconds") ]
    ]

formatTime : Time -> String
formatTime time =
  Time.inSeconds time
    |> toString

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TimeUpdate time ->
      ( { model | currentTime = time }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions =
  Time.every Time.second TimeUpdate
```

And now our test should pass! Notice that we were able to test drive our component without
our tests knowing how that component actually deals with the effect from the Time
subscription.

### Deveopment

For development purposes, it's possible to deploy Elmer to a local project.

First, install [elm-ops-tooling](https://github.com/NoRedInk/elm-ops-tooling); you'll use this to install Elmer. Next, you'll want to set up your project with [elm-test](https://github.com/elm-community/elm-test). Since you'll be deploying Elmer manually, you'll have to make sure its dependencies are available in your tests. So, check the `dependencies` section of `elm-package.json` in your tests directory to be sure that it contains the following:

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

To run the tests:

```
$ elm test
```
