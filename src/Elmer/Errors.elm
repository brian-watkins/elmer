module Elmer.Errors exposing
  ( CustomError
  , failWith
  , print
  , noModel
  , noTitle
  , wrongTitle
  , noLocation
  , wrongLocation
  , sendUrlRequiresApplication
  , badUrl
  , navigationSpyRequiresApplication
  , elementNotFound
  , wrongAttribute
  , wrongAttributeName
  , noAttribute
  , noRequest
  , wrongRequest
  , requestMatcherFailed
  )

{-| Exposed for testing

@docs CustomError, print, failWith
@docs noModel, noTitle, wrongTitle, noLocation, wrongLocation, sendUrlRequiresApplication
@docs badUrl, navigationSpyRequiresApplication, elementNotFound, wrongAttribute, wrongAttributeName, noAttribute
@docs noRequest, wrongRequest, requestMatcherFailed

-}

import Expect exposing (Expectation)
import Elmer.Printer exposing (..)
import Dict exposing (Dict)

{-|
-}
type alias CustomError
  = List Message

{-|
-}
noModel : CustomError
noModel =
  [ description "No model! Call Elmer.init to provide a model."
  ]


{-|
-}
wrongTitle : String -> String -> CustomError
wrongTitle expected actual =
  [ message "Expected document to have title" expected
  , message "but it has" actual
  ]


{-|
-}
noTitle : String -> CustomError
noTitle expected =
  [ message "Expected document to have title" expected
  , description "but the supplied view function does not result in a Document value"
  ]


{-|
-}
noLocation : String -> CustomError
noLocation expected =
  [ message "Expected to be at location:" expected
  , description "but no location has been set"
  ]


{-|
-}
wrongLocation : String -> String -> CustomError
wrongLocation expected actual =
  [ message "Expected to be at location:" expected
  , message "but location is:" actual
  ]


{-|
-}
sendUrlRequiresApplication : CustomError
sendUrlRequiresApplication =
  [ description "sendUrlRequest can only be used when testing an Elm Html application."
  , description "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
badUrl : String -> String -> CustomError
badUrl fun expected =
  [ message ("Fake " ++ fun ++ " could not process url") expected
  , description "because it does not appear to be a url"
  ]


{-|
-}
navigationSpyRequiresApplication : String -> String -> CustomError
navigationSpyRequiresApplication fun expected =
  [ message ("Fake " ++ fun ++ " could not process url") expected
  , description "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
elementNotFound : String -> String -> CustomError
elementNotFound selector dom =
  [ message "The targeted html element was not found" selector
  , message "The current view is" dom
  ]


{-|
-}
wrongAttribute : String -> String -> String -> CustomError
wrongAttribute property expectedValue actualValue =
  [ message "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , message "but it has" <| property ++ " = " ++ actualValue
  ]


{-|
-}
noAttribute : String -> String -> CustomError
noAttribute property expectedValue =
  [ message "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , description "but it has no attribute with that name"
  ]

{-|
-}
wrongAttributeName : String -> String -> Dict String String -> CustomError
wrongAttributeName property expectedValue attrs =
  let
      messages =
        Dict.toList attrs
          |> List.map (\(key, val) -> key ++ " = " ++ val)
          |> String.join "\n"
  in
  [ message "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , message "but it has these attributes" messages ]


{-|
-}
noRequest : String -> CustomError
noRequest expectedRoute =
  [ message "Expected request for" expectedRoute
  , description "but no requests have been made"
  ]


{-|
-}
wrongRequest : String -> String -> CustomError
wrongRequest expectedRoute actualRequests =
  [ message "Expected request for" expectedRoute
  , message "but only found these requests" actualRequests
  ]

{-|
-}
requestMatcherFailed : String -> String -> CustomError
requestMatcherFailed expectedRoute failure =
  [ message "Requests matching" expectedRoute
  , description "failed to meet the expectations:"
  , description failure
  ]

{-|
-}
failWith : CustomError -> Expectation
failWith =
  Expect.fail << print

{-|
-}
print : CustomError -> String
print =
  format