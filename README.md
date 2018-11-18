# Elmer

Elmer makes it easy to describe the behavior of Elm HTML applications. If you love TDD and
you love Elm, then you'll probably appreciate Elmer.

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

## Getting Started

Because Elmer uses some native Javascript code to accomplish its magic, you cannot install Elmer through the elm package repository. Instead, you can install Elmer with the `elmer-test` package on NPM. Follow these steps to TDD bliss ...

### Install

First, you'll need to install 
- Elm (0.19)
- The latest version of the [node test runner](https://www.npmjs.com/package/elm-test)
for the elm test package that works with Elm 0.19 (`elm-test@elm0.19.0`)
- This package, which you'll install via npm

I recommend installing these dependencies locally in your project directory so you can track versions carefully. Here's the command to install all these at once:

```
$ npm install --save-dev elm elm-test@elm0.19.0 elmer-test
```

Now install the elm test library:

```
$ npx elm install elm-explorations/test
```

### Update the elm.json file

In your `elm.json` file, you'll need to manually add elmer to the `test-dependencies` section like so:

```
"test-dependencies": {
  "direct": {
    "elm-explorations/test": "1.1.0",
    "elm-explorations/elmer": "5.0.0"
  },
  "indirect": {}
}
```

The latest version of Elmer is 5.0.0. Make sure the version number of elmer 
matches the version number of the `elmer-test` NPM package.

Notice the `indirect` section under `test-dependencies`. Elmer itself has the following dependencies:

```
"dependencies": {
  "elm/browser": "1.0.0 <= v < 2.0.0",
  "elm/core": "1.0.0 <= v < 2.0.0",
  "elm/html": "1.0.0 <= v < 2.0.0",
  "elm/http": "1.0.0 <= v < 2.0.0",
  "elm/json": "1.0.0 <= v < 2.0.0",
  "elm/random": "1.0.0 <= v < 2.0.0",
  "elm/url": "1.0.0 <= v < 2.0.0",
  "elm-explorations/test": "1.0.0 <= v < 2.0.0"
},
```

If any of these dependencies are not already listed as direct or indirect dependencies of your app, you'll need to list these in the `indirect` section of your `test-dependencies`. 

If you just try to run elm-test (see below) and you're missing any dependencies, the compiler will give you an error message. Take the missing dependencies it mentions and list them as indirect test dependencies.

For example, here's what the `test-dependencies` would look like for an app that doesn't use `elm/http`:

```
"test-dependencies": {
  "direct": {
    "elm-explorations/test": "1.1.0",
    "elm-explorations/elmer": "5.0.0"
  },
  "indirect": {
    "elm/http": "1.0.0"
  }
}
```

### Run

Now that everything's in place, you're ready to write tests with Elmer. In order to run those tests, you'll need to set the `ELM_HOME` environment variable to the `home` directory under the `elmer-test` install. If you've installed `elmer-test` locally, the directory should look like this:

```
<Project Home>/node_modules/elmer-test/home
```

I recommend adding a test script to your `package.json` that sets the environment variable for you. The following will work on a Mac running bash:

```
"scripts": {
  "test": "ELM_HOME=$(pwd)/node_modules/elmer-test/home elm-test"
}
```

Note that `ELM_HOME` must be an absolute path (thus the `$(pwd)` in the test command).

### Caveats

The `elm` command searches for test dependencies any time you invoke it (so, even if you aren't running tests). This means that you will need to set the `ELM_HOME` environment variable as described above, any time you invoke the `elm` command. For example, to build your app, you'll need to do something like:

```
$ ELM_HOME=$(pwd)/node_modules/elmer-test/home elm make src/Main.elm
```

## Releases

#### 5.0.0
- Revised `Elmer.Spy` api to make it simpler and to allow the compiler to do type checking when injecting a spy or providing a fake implementation. This should provide better feedback when working with spies
- See the section at the end of this document on migrating tests from 4.0.0 to 5.0.0

#### 4.0.0
- Updated Elmer to work with Elm 0.19
- Revised api for targeting Html elements to allow the compiler to provide better feedback
- See the section at the end of this document on migrating tests from 3.3.1 to 4.0.0

#### 3.3.1
- Last version for Elm 0.18


## Documentation

Read the [latest documentation](https://elmer-test.cfapps.io/).

If you're interested in Elmer for Elm 0.18, you should read the documentation for Elmer 3.3.1, which
you can find [here](https://elmer-test.cfapps.io/versions).

## Describing Behavior

While tests can be written with Elmer in a variety of ways, the goal is use Elmer to describe *behavior*
rather than implementation details. What do I mean? Let's say that an application is a collection of 
behaviors. Each behavior has some pre-conditions -- these are characteristics of the world *outside* the 
application that must be true for the behavior to occur -- and some set of resulting post-conditions --
characteristics of the world *outside* the application that are a consequence of the behavior. To
describe the *behavior* of an application, then, is to describe all the relationships that hold between
relevant states of the world outside the application due to the use of that application. 

Here's an example of a behavior from some game application that displays high scores:

- Given that there is an HTTP web service that responds with a 200 status and a JSON document
that lists the high scores in some format.
- When the user starts the game application, then the high scores are displayed as list items in HTML.

This behavior links one state of the world -- where there is an HTTP web service that successfully
returns a JSON document in some known format -- and another -- where some HTML document contains
several `<li>` elements whose text shows the high scores from the web service. 

To describe this behavior, we should not care how the application accomplishes the mapping between
these two states. We only care about describing the two states. To make the test pass, we will need
to provide some implementation, but the test gives us freedom to choose whatever implementation makes
sense for us now. Most importantly, however, as we add new behaviors to our application, we will be
able to refactor our code with confidence. No matter what implementation we end up with, we should
still be able to run this test and ensure that the same mapping between pre-conditions and post-conditions
still holds. 

Like I've said, you can use Elmer to write tests in a variety of ways, but I encourage you to
write tests that describe *behavior* so that you can refactor your code with confidence later on. This means
your tests should know as little as they can about the implementation of your Elm application. Strive to write
tests that do not know the shape of your model or the particular messages that flow through the update function.
Don't unit test functions. Begin each test only with references to the functions that must exist -- `view`, `update`,
`init` -- and use Elmer to describe the pre- and post-conditions associated with some behavior. 

### Create a TestState

To begin a test with Elmer, you need to generate a `TestState` value. There are a variety of ways to do this:

- Use `givenElement`, `givenApplication`, `givenDocument`, or `givenWorker` from
the `Elmer.Program` module to test particular kinds of programs. In these cases, you'll
use `Elmer.Program.init` to provide an initial model and command.
- Use `Elmer.given` to test an arbitrary model, view method, and update method.
- Use `Elmer.Command.given` to test a command-generating function in isolation.

### Working with HTML

Since Elm is primarily designed for writing HTML applications, much of the work that goes into describing
the pre- and post-conditions that characterize some behavior will involve working with HTML elements.

Elmer allows you to simulate events on elements and examine the state of elements. In order to do either, 
you'll need to first target an element.

#### Targeting an Element

Use `Elmer.Html.target` along with the functions from `Elmer.Html.Selector` to target an element. Here's
a partial test that targets all the `<li>` elements that are children of an `<ol>` with a class `scores`
in the current view:

```
allTests : Test
allTests =
  describe "My Fun Game"
  [ describe "High Score Screen"
    [ test "it shows the high scores" <|
      \() ->
        Elmer.Program.givenElement App.view App.update
          |> Elmer.Program.init (\_ -> App.init testFlags)
          |> Elmer.Html.target
              << Elmer.Html.Selector.childrenOf 
                [ Elmer.Html.Selector.tag "ol"
                , Elmer.Html.Selector.class "score-list"
                ]
              << Elmer.Html.Selector.by
                [ Elmer.Html.Selector.tag "li" ]
    ...
```

See `Elmer.Html.Selector` for more examples of selectors. It's also possible to write your own.

#### Taking action on an element

Once you target an element, that element is the subject of subsequent actions, until
you target another element. The following functions define actions on elements:

+ Click events: `Elmer.Html.Event.click <testState>`
+ Input events: `Elmer.Html.Event.input <text> <testState>`
+ Custom events: `Elmer.Html.Event.trigger <eventName> <eventJson> <testState>`
+ There are also events for mouse movements, and checking and selecting input elements. See
the [docs](https://elmer-test.cfapps.io/) for more information.

#### Element Matchers

You can make expectations about targeted elements with the `Elmer.Html.expect` function.

First, specify whether you want to match against a single element (with `element`)
or a list of elements (with `elements`).
Then you provide the appropriate matchers for the element or the list. You can also
expect that an element exists with the `elementExists` matcher.

See `Elmer.Html.Matchers` for a full list of matchers. Let's add to the example above to make
an expectation about the elements we targeted. We'll use `Elmer.expectAll` to chain together several
assertions. We'll expect that the list of `<li>` we've targeted has 2 elements, with the first containing
text of "700 Points" and the second "900 Points". 

```
allTests : Test
allTests =
  describe "My Fun Game"
  [ describe "High Score Screen"
    [ test "it shows the high scores" <|
      \() ->
        Elmer.Program.givenElement App.view App.update
          |> Elmer.Program.init (\_ -> App.init testFlags)
          |> Elmer.Html.target
              << Elmer.Html.Selector.childrenOf 
                [ Elmer.Html.Selector.tag "ol"
                , Elmer.Html.Selector.class "score-list"
                ]
              << Elmer.Html.Selector.by
                [ Elmer.Html.Selector.tag "li" ]
          |> Elmer.Html.expect (Elmer.Html.Matchers.elements <|
              Elmer.expectAll
              [ Elmer.hasLength 2
              , Elmer.atIndex 0 <| Elmer.Html.Matchers.hasText "700 Points"
              , Elmer.atIndex 1 <| Elmer.Html.Matchers.hasText "900 Points"
              ]
            )
    ]
  ]
```

### Commands

Commands describe actions to be performed by the Elm runtime; the result of a command depends on the state of the
world outside the Elm application. Elmer simulates the Elm runtime in order to
facilitate testing, but it is not intended to replicate the Elm runtime's ability to carry out commands.
Instead, Elmer allows you to specify what effect should result from running a command. This is one important
way that Elmer allows you to describe the pre-conditions that characterize an application behavior.

#### Faking Effects

Suppose there is a function `f : a -> b -> Cmd msg` that takes two arguments and produces a command. In order
to specify the effect of this command in our tests, we will *override* occurrences of `f`
with a function we create in our tests. This function will generate a special command
that specifies the intended effect, and Elmer will process the result as if the original command were actually performed.

For a more concrete example, check out this [article](https://medium.com/@brian.watkins/test-driving-elm-with-elmer-649e2e7e02a8), which discusses how to fake
commands and subscriptions during a test.

Note that while Elmer is not capable of processing any commands, it does support
the general operations on commands in the core `Platform.Cmd` module, namely, `batch` and `map`. So, you
can use these functions as expected in your application and Elmer should do the right thing.

Elmer provides additional support for HTTP request commands and navigation commands.

#### Elmer.Http

Modern web apps often need to make HTTP requests to some backend server. Elmer makes it easy to stub HTTP
responses and write expectations about the requests made. The `Elmer.Http.Stub` module contains methods
for constructing an `HttpResponseStub` that describes how to respond to some request. For example,
we might stub a request to the server for our game to return high scores like so:

```
let
  stubbedResponse = Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fakeGameServer.com/scores")
    |> Elmer.Http.Stub.withBody "[{\"score\":700,\"player\":\"Brian\"},{\"score\":900,\"player\":\"Holly\"}]"
in
```

In this case, a GET request to the given route will result in a response with the given body.
See `Elmer.Http.Stub` for the full list of builder functions.

Once an `HttpResponseStub` has been created, you can use the `Elmer.Http.serve` function
along with `Elmer.Spy.use` to override `Http.send` and `Http.toTask` from [elm/http](https://package.elm-lang.org/packages/elm/http/latest/) during your test.
When your application code calls `Http.send` or `Http.toTask`, the request will be checked against the
provided stubs and if a match occurs, the given response will be returned.

Here's how we can extend our test to describe more of its pre-conditions:

```
allTests : Test
allTests =
  describe "My Fun Game"
  [ describe "High Score Screen"
    [ test "it shows the high scores" <|
      \() ->
        let
          stubbedResponse =
            Elmer.Http.Stub.for (Elmer.Http.Route.get "http://fakeGameServer.com/scores")
              |> Elmer.Http.Stub.withBody 
                "[{\"score\":700,\"player\":\"Brian\"},{\"score\":900,\"player\":\"Holly\"}]"
        in
          Elmer.Program.givenElement App.view App.update
            |> Elmer.Spy.use [ Elmer.Http.serve [ stubbedResponse ] ]
            |> Elmer.Program.init (\_ -> App.init testFlags)
            |> Elmer.Html.target
                << Elmer.Html.Selector.childrenOf 
                  [ Elmer.Html.Selector.tag "ol"
                  , Elmer.Html.Selector.class "score-list"
                  ]
                << Elmer.Html.Selector.by
                  [ Elmer.Html.Selector.tag "li" ]
            |> Elmer.Html.expect (Elmer.Html.Matchers.elements <|
                Elmer.expectAll
                [ Elmer.hasLength 2
                , Elmer.atIndex 0 <| Elmer.Html.Matchers.hasText "700 Points"
                , Elmer.atIndex 1 <| Elmer.Html.Matchers.hasText "900 Points"
                ]
              )
    ]
  ]
```

Elmer also allows you to write tests that expect some HTTP request to have been made, in a
manner similar to how you can write expectations about some element in an HTML document. For
example, this test inputs search terms into a field, clicks a search button, and then expects
that a request is made to a specific route with the search terms in the query string:

```
Elmer.given App.defaultModel App.view App.update
  |> Elmer.Spy.use [ Elmer.Http.serve [ stubbedResponse ] ]
  |> Elmer.Html.target << by [ tag "input", attribute ("name", "query") ]
  |> Elmer.Html.Event.input "Fun Stuff"
  |> Elmer.Html.target << by [ id "search-button" ]
  |> Elmer.Html.Event.click
  |> Elmer.Http.expect (Elmer.Http.Route.get "http://fake.com/search") (
    Elmer.some <| Elmer.Http.Matchers.hasQueryParam ("q", "Fun Stuff")
  )
```

If you don't care to describe the behavior of your app after the response from a request is
received -- that is, if you don't care to create a stubbed response for some request -- you
can provide `Elmer.Http.spy` to `Elmer.Spy.use` and it will override the `Http.send` and `Http.toTask`
functions so that they merely record any requests received.

See `Elmer.Http` and `Elmer.Http.Matchers` for more.

#### Elmer.Navigation

Elmer provides support for functions in the [Browser.Navigation](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation)
module that allow you to handle navigation for single-page web applications.

You'll need to begin your test with `Elmer.Program.givenApplication` since only Elm
'application' programs can handle navigation. Provide a reference to the messages
that handle new url requests and url changes along with the view and update functions. Then 
provide `Elmer.Spy.use` with `Elmer.Navigation.spy`
so that Elmer will be able to record and process location updates by overriding
 `Browser.Navigation.pushUrl` and `Browser.Navigation.replaceUrl`.

When you call `Elmer.Program.init` you'll need to use `Elmer.Navigation.fakeKey` to
give your `init` function a `Browser.Navigation.Key` value. Here's an example of a test
that expects the location to change when an element is clicked. 

```
Elmer.Program.givenApplication App.OnUrlRequest App.OnUrlChange App.view App.update
  |> Elmer.Spy.use [ Elmer.Navigation.spy ]
  |> Elmer.Program.init (\_ -> App.init testFlags testUrl Elmer.Navigation.fakeKey)
  |> Elmer.Html.target << by [ id "some-element" ]
  |> Elmer.Html.Event.click
  |> Elmer.Navigation.expectLocation "http://mydomain.com/funStuff.html"
```

You can write an expectation about the current location with `Elmer.Navigation.expectLocation`.

See `tests/src/Elmer/TestApps/NavigationTestApp.elm` and `tests/src/Elmer/NavigationTests.elm` for
examples.

#### Deferred Command Processing

It's often necessary to describe the behavior of an application while some command is running. For example,
one might want to show a progress indicator while an HTTP request is in process. Elmer provides
general support for deferred commands. Use `Elmer.Command.defer` to create a command that
will not be processed until `Elmer.Command.resolveDeferred` is called. Note that all currently
deferred commands will be resolved when this function is called.

`Elmer.Http` allows you to specify when the processing of a stubbed response should be deferred.
When you create your `HttpResponseStub` just use the `Elmer.Http.Stub.deferResponse` builder function
to indicate that this response should be deferred until `Elmer.Command.resolveDeferred` is called.

#### Testing Commands in Isolation

You might want to test a command independently of any module that might use it. In that case,
use `Elmer.Command.given` and provide it with a function that generates the command you
want to test. This will initiate a `TestState` that simply records any messages that result when
the given command is processed. You can use the `Elmer.Command.expectMessages` function to
make any expectations about the messages received. For example, here's a test that expects a
certain message when a certain command is processed:

```
Elmer.Command.given (\_ -> MyModule.myCommand MyTagger withSomeArgument)
  |> Elmer.Command.expectMessages (\messages ->
    Expect.equal [ MyTagger "Fun Result" ]
  )
```

You can use `Elmer.Command.given` with other functions as it makes sense. So, you might
write a test that expects a certain Http request to result from the processing of a command:

```
Elmer.Command.given (\_ -> MyModule.sendRequest MyTagger someArgument)
  |> Elmer.Spy.use [ Elmer.Http.spy ]
  |> Elmer.Http.expectRequest (Elmer.Http.Route.get "http://fun.com/api/someArgument")
```

### Subscriptions

Using subscriptions, your application can register to be notified when certain effects occur.
To describe the behavior of an application that has subscriptions, you'll need to do these things:

1. Override the function that generates the subscription using `Elmer.Spy.create` along with
`Elmer.Spy.andCallFake`
and replace it with a fake subscription using `Elmer.Subscription.fake`
2. Register the subscriptions using `Elmer.Subscription.with`
2. Simulate the effect you've subscribed to receive with `Elmer.Subscription.send`

Here's an example test:

```
timeSubscriptionTest : Test
timeSubscriptionTest =
  describe "when a time effect is received"
  [ test "it prints the number of seconds" <|
    \() ->
      let
        timeSpy =
          Elmer.Spy.observe (\_ -> Time.every)
            |> Elmer.Spy.andCallFake (\_ tagger ->
              Elmer.Subscription.fake "timeEffect" tagger
            )
      in
        Elmer.given App.defaultModel App.view App.update
          |> Elmer.Spy.use [ timeSpy ]
          |> Elmer.Subscription.with (\() -> App.subscriptions)
          |> Elmer.Subscription.send "timeEffect" (Time.millisToPosix 3000)
          |> Elmer.Html.target << by [ id "num-seconds" ]
          |> Elmer.Html.expect (
              Elmer.Html.Matchers.element <|
                Elmer.Html.Matchers.hasText "3 seconds"
             )
  ]
```

For a more complete example, check out this [article](https://medium.com/@brian.watkins/test-driving-elm-with-elmer-649e2e7e02a8).

### Ports

You can manage ports during your test in just the same way you would manage any
command or subscription.

Suppose you have a port that sends data to Javascript:

```
port module MyModule exposing (..)

port sendData : String -> Cmd msg
```

You can create a spy for this function just like you would for any command-generating
function:

```
Elmer.Spy.observe (\_ -> MyModule.sendData)
  |> Elmer.Spy.andCallFake (\_ -> Cmd.none)
```

Note that you will need to provide a fake implementation of this method since
otherwise Elmer will not know how to handle the generated command.

A port that receives data from Javascript works just the same as any subscription.

```
port receiveData : (String -> msg) -> Sub msg

type Msg = ReceivedData String

subscriptions : Module -> Sub Msg
subscriptions model =
  receiveData ReceivedData
```

We can create a spy for this subscription-generating function and provide a
fake subscription that will allow us to send data tagged with the appropriate message
during our test.

```
let
  spy =
    Elmer.Spy.observe (\_ -> MyModule.receiveData)
      |> Elmer.Spy.andCallFake (\tagger ->
           Elmer.Subscription.fake "fake-receive" tagger
         )
in
  Elmer.given MyModule.defaultModel MyModule.view MyModule.update
    |> Elmer.Spy.use [ spy ]
    |> Elmer.Subscription.with (\_ -> MyModule.subscriptions)
    |> Elmer.Subscription.send "fake-receive" "some fake data"
    |> ...
```

### Testing Tasks

Elm uses tasks to describe asynchronous operations at a high-level. You can use Elmer
to describe the behavior of applications that use the Task API. To do so:

1. Stub any task-generating functions to return a task created with `Task.succeed`
or `Task.fail` and the value you want as necessary for the behavior you want to describe.

2. That's it.

Elmer does not know how to run any tasks other than `Task.succeed` and `Task.fail`.
However, Elmer does know how to properly apply all the functions from the Task API.
In this way, Elmer allows you to describe the behavior that results from operations
with tasks without actually running those tasks during your test.

Here's an example. Suppose when a button is clicked, your app creates a task that gets
the current time, formats it (using some function called `formatTime : Time -> String`),
and tags the resulting string with `TagFormattedTime` like so:

    Time.now
      |> Task.map formatTime
      |> Task.perform TagFormattedTime

You can test this behavior by replacing `Time.now` with a `Task.succeed`
that resolves to the time you want.

    let
      timeSpy =
        Task.succeed (Time.millisToPosix 1515281017615)
          |> Spy.replaceValue (\_ -> Time.now)
    in
      testState
        |> Elmer.Spy.use [ timeSpy ]
        |> Elmer.Html.target << by [ id "get-current-time" ]
        |> Elmer.Html.Event.click
        |> Elmer.Html.target << by [ id "current-time" ]
        |> Elmer.Html.expect (
          element <| hasText "1/6/2018 23:23:37"
        )

### Spies and Fakes

Elmer generalizes the pattern for managing the effects of `Subs` and `Cmds`, allowing
you to spy on any function you like. *NOTE* You should use Elmer spies sparingly and
with care. Each spy that you add to your test couples that test to implementation details. 

Suppose you need to write a test that expects a certain function to be called, but
you don't need to describe the resulting behavior. You can spy on a function with
`Elmer.Spy.observe` and make expectations about it with `Elmer.Spy.expect`.

For example, suppose you want to ensure that a component is calling a specific
function in another module for parsing some string. You have tests for the
parsing function itself; you just need to know that your component is using it.

```
parseTest : Test
parseTest =
  describe "when the string is submitted"
  [ test "it passes it to the parsing module" <|
    \() ->
      let
        spy = 
          Elmer.Spy.observe (\_ -> MyParserModule.parse)
            |> Elmer.Spy.andCallThrough
      in
        Elmer.given App.defaultModel App.view App.update
          |> Elmer.Spy.use [ spy ]
          |> Elmer.Html.target << by [ tag "input", attribute ("type", "text") ]
          |> Elmer.Html.Event.input "A string to be parsed"
          |> Elmer.Spy.expect (\_ -> MyParserModule.parse) (
            wasCalled 1
          )
  ]
```

Elmer also allows you to provide a fake implementation for any function.
Suppose that you want to stub the result of the parsing function:

```
parseTest : Test
parseTest =
  describe "when the string is submitted"
  [ test "it displays the parsed result" <|
    \() ->
      let
        spy =
          Elmer.Spy.observe (\_ -> MyParserModule.parse)
            |> Elmer.Spy.andCallFake (\_ ->
              "Some Parsed String"
            )
      in
        Elmer.given App.defaultModel App.view App.update
          |> Elmer.Spy.use [ spy ]
          |> Elmer.Html.target << by [ tag "input", attribute ("type", "text") ]
          |> Elmer.Html.Event.input "A string to be parsed"
          |> Elmer.Html.target << by [ id "parsing-result" ]
          |> Elmer.Html.expect (Elmer.Html.element <|
            Elmer.Html.Matchers.hasText "Some Parsed String
          )
  ]
```

For any spy, you can make an expectation about how many times it was called like so:

```
Elmer.Spy.expect (\_ -> MyModule.someFunction) (wasCalled 3)
```

You can also expect that the spy was called with some list of arguments at least once:

```
Elmer.Spy.expect (\_ -> MyModule.someFunction) (
  Elmer.Spy.Matchers.wasCalledWith
    [ Elmer.Spy.Matchers.stringArg "someString"
    , Elmer.Spy.Matchers.anyArg
    , Elmer.Spy.Matcher.intArg 23
    ]
)
```

See `Elmer.Spy.Matchers` for a full list of argument matchers.

`Elmer.Spy.observe` is good for spying on named functions in your production code.
Sometimes, though, it would be nice to provide the code you are testing with a 'fake' function
for testing purposes only. Suppose that you are testing a module that takes a function
as an argument, and you want to expect that the function is called with a certain argument.
You can create a 'fake' function in your test module, observe it with a spy, and provide it to
the code you're testing using `Elmer.Spy.inject`. For example:

```
someFake tagger data =
  Command.fake <| tagger data

myTest =
  let
    spy =
      Spy.observe (\_ -> someFake)
        |> Spy.andCallThrough
  in
    Elmer.given testModel MyModule.view (MyModule.updateUsing <| Spy.inject (\_ -> someFake))
      |> Elmer.Spy.use [ spy ]
      |> Elmer.Html.target << by [ tag "input" ]
      |> Elmer.Html.Event.input "some text"
      |> Elmer.Html.target << by [ tag "button" ]
      |> Elmer.Html.Event.click
      |> Elmer.Spy.expect (\_ -> someFake) (
        Elmer.Spy.Matchers.wasCalledWith
          [ Elmer.Spy.Matchers.anyArg
          , Elmer.Spy.Matchers.stringArg "some text"
          ]
      )
```

Use `Elmer.Spy.inject` to provide the function you want to observe so that Elmer has time to install the associated spy during the test.

Finally, you can use `Spy.replaceValue` to replace the value returned by a no-argument function
(such as `Time.now`) during a test. You can't make expectations about spies created in this
way; `Spy.replaceValue` is just a convenient way to inject fake values during a test.


### Upgrading from Elmer 4.x

If you've written tests with Elmer 4.x, the `Elmer.Spy` api has changed:

- `Elmer.Spy.create` is now `Elmer.Spy.observe` and no longer needs a string identifier.

- When using `Elmer.Spy.observe` you must call either `Elmer.Spy.andCallThrough` or `Elmer.Spy.andCallFake` so
that Elmer knows what to do when the observed function is called.

- `Elmer.Spy.expect` now uses a reference to the observed function (rather than a string) to identify the function
you'd like to assert about. 

- `Elmer.Spy.createWith` has been removed. Create a 'fake' function in your test module and observe it with `Elmer.Spy.observe` instead.

- `Elmer.Spy.callable` has been removed. Instead, use `Elmer.Spy.inject` to provide a 'fake' function to the code under test. 


### Upgrading from Elmer 3.x

If you've written tests with Elmer 3.x and plan to upgrade them to Elmer 4.0.0, here are some things
you'll need to consider:

- `Elmer.Platform.Command` has changed to `Elmer.Command`.

- `Elmer.Platform.Subscription` has changed to `Elmer.Subscription`.

- `Elmer.Headless.givenCommand` has been replaced with `Elmer.Command.given`.

- `Elmer.Headless.expectMessages` has been replaced with `Elmer.Command.expectMessages`.

- `Elmer.Headless.given` has been replaced with `Elmer.Program.givenWorker`.

- `Elmer.Http.expectThat` has been replaced with `Elmer.Http.expect`.

- `Elmer.Http.expect` has been replaced with `Elmer.Http.expectRequest`.

- The `<&&>` operator has been replaced with `Elmer.expectAll`.

- `Elmer.Html.Matchers.hasProperty` has been removed. Use `Elmer.Html.Matchers.hasAttribute` instead.

- `Elmer.Html.target` now has a new api. Combine functions in the `Elmer.Html.Selector` module to build a an element selector.


### Development

To run the tests:

```
$ cd tests
$ ./test.sh
```
