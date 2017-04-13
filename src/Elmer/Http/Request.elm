module Elmer.Http.Request exposing
  ( HttpRequest
  , body
  )

{-| Functions for working with a recorded HTTP request.

@docs HttpRequest, body

-}

import Elmer.Http.Internal as Internal

{-| Represents a recorded HTTP request about which expectations may be made.
-}
type alias HttpRequest
  = Internal.HttpRequest


{-| Get the body of a recorded HTTP request.
-}
body : HttpRequest -> Maybe String
body request =
  request.body
