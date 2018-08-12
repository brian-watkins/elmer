module Elmer.Http.Route exposing
  ( HttpRoute
  , get
  , post
  , put
  , patch
  , delete
  , method
  , url
  )

{-| Functions for working with HTTP routes.

A route is an HTTP request method plus a URL, up to but not including a query string.

# Creating an HttpRoute
@docs HttpRoute, get, post, delete, put, patch

# Accessing an HttpRoute
@docs method, url

-}

import Elmer.Http.Types as Types

{-| Represents an HTTP route, which is a URL plus an HTTP request method.
-}
type alias HttpRoute =
  Types.HttpRoute

{-| Create a route for an HTTP GET.

    get "http://funstuff.com"

If the given url contains a query string, it will be ignored.
-}
get : String -> HttpRoute
get =
  make "GET"

{-| Create a route for an HTTP POST.

    post "http://funstuff.com"

If the given url contains a query string, it will be ignored.
-}
post : String -> HttpRoute
post =
  make "POST"

{-| Create a route for an HTTP PUT.

    put "http://funstuff.com"

If the given url contains a query string, it will be ignored.
-}
put : String -> HttpRoute
put =
  make "PUT"

{-| Create a route for an HTTP PATCH.

    patch "http://funstuff.com"

If the given url contains a query string, it will be ignored.
-}
patch : String -> HttpRoute
patch =
  make "PATCH"

{-| Create a route for an HTTP DELETE.

    delete "http://funstuff.com"

If the given url contains a query string, it will be ignored.
-}
delete : String -> HttpRoute
delete =
  make "DELETE"

make : String -> String -> HttpRoute
make methodValue urlValue =
  let
    cleanUrl =
      String.split "?" urlValue
        |> List.head
        |> Maybe.withDefault ""
  in
    { method = methodValue
    , url = cleanUrl
    }

{-| Return the route's method.
-}
method : HttpRoute -> String
method route =
  route.method

{-| Return the route's url.
-}
url : HttpRoute -> String
url route =
  route.url
