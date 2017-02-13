module Elmer.Http.Status exposing
  ( HttpStatus
  , httpStatus
  , ok
  , created
  , serverError
  , notFound
  , unauthorized
  , forbidden
  )

{-| Functions for working with Http status codes.

# General
@docs HttpStatus, httpStatus

# Common Status Codes
@docs ok, created, unauthorized, forbidden, notFound, serverError

-}

import Elmer.Http.Internal as Internal


{-| Represents the status of an Http response
-}
type alias HttpStatus =
  Internal.HttpStatus


{-| Generate an `HttpStatus`.
-}
httpStatus : Int -> String -> HttpStatus
httpStatus code message =
  Internal.HttpStatus
    { code = code
    , message = message
    }


{-| The `200 OK` Http status.
-}
ok : HttpStatus
ok =
  httpStatus 200 "Ok"


{-| The `201 Created` Http status.
-}
created : HttpStatus
created =
  httpStatus 201 "Created"


{-| The `404 Not Found` Http status.
-}
notFound : HttpStatus
notFound =
  httpStatus 404 "Not Found"


{-| The `500 Internal Server Error` Http status.
-}
serverError : HttpStatus
serverError =
  httpStatus 500 "Internal Server Error"


{-| The `401 Unauthorized` Http status.
-}
unauthorized : HttpStatus
unauthorized =
  httpStatus 401 "Unauthorized"


{-| The `403 Forbidden` Http status.
-}
forbidden : HttpStatus
forbidden =
  httpStatus 403 "Forbidden"
