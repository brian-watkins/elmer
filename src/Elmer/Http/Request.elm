module Elmer.Http.Request exposing
  ( HttpRequest
  , body
  , queryString
  )

{-| Functions for working with a recorded HTTP request.

@docs HttpRequest, body, queryString

-}

import Elmer.Http.Internal as Http_

{-| Represents a recorded HTTP request about which expectations may be made.
-}
type alias HttpRequest
  = Http_.HttpRequest


{-| Get the body of a recorded HTTP request.
-}
body : HttpRequest -> Maybe String
body request =
  request.body


{-| Get the query string of a recorded HTTP request, if it exists
-}
queryString : HttpRequest -> Maybe String
queryString request =
  Http_.queryString request.url
