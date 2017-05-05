module Elmer.HttpRouteTests exposing (all)

import Test exposing (..)
import Expect
import Elmer.Http.Route as Route
import Elmer.Http.Internal exposing (HttpRoute)

all : Test
all =
  describe "HttpRoute"
  [ methodTests "GET" Route.get
  , methodTests "POST" Route.post
  , methodTests "PUT" Route.put
  , methodTests "PATCH" Route.patch
  , methodTests "DELETE" Route.delete
  ]

methodTests : String -> (String -> HttpRoute) -> Test
methodTests method func =
  let
    route = func "http://fun.com/fun.html"
  in
    describe (method ++ " tests")
    [ test "it produces a route with the proper method" <|
      \() ->
        Route.method route
          |> Expect.equal method
    , test "it produces a route with the given url" <|
      \() ->
        Route.url route
          |> Expect.equal "http://fun.com/fun.html"
    , describe "when there is a query string on the url"
      [ test "it removes the query string" <|
        \() ->
          let
            queryStringRoute = func "http://fun.com/fun.html?queryParam=superCool"
          in
            Route.url queryStringRoute
              |> Expect.equal "http://fun.com/fun.html"
      ]
    ]
