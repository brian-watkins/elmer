module Elmer.Http.Request exposing
  ( HttpRequest
  , body
  , queryString
  , headers
  )

{-| Functions for working with a recorded HTTP request.

@docs HttpRequest, body, queryString, headers

-}

import Elmer.Http.Internal as Http_
import Elmer.Http.Types as Types

{-| Represents a recorded HTTP request about which expectations may be made.
-}
type alias HttpRequest
  = Types.HttpRequest


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


{-| Get the headers of a recorded HTTP request as a list of (name, value) tuples. 
-}
headers : HttpRequest -> List (String, String)
headers request =
  request.headers
    |> List.map (\header -> (header.name, header.value))