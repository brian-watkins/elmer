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
  , failedToActivateSpies
  , unknownSpy
  , wrongNumberOfSpyCalls
  , badSpyIdentifier
  , eventHandlerNotFound
  , noElementTargetedForEvent
  )

{-| Exposed for testing

@docs CustomError, print, failWith
@docs noModel, noTitle, wrongTitle, noLocation, wrongLocation, sendUrlRequiresApplication
@docs badUrl, navigationSpyRequiresApplication, elementNotFound, wrongAttribute, wrongAttributeName, noAttribute
@docs noRequest, wrongRequest, requestMatcherFailed, failedToActivateSpies, unknownSpy, wrongNumberOfSpyCalls
@docs badSpyIdentifier, eventHandlerNotFound, noElementTargetedForEvent

-}

import Expect exposing (Expectation)
import Elmer.Message exposing (..)
import Dict exposing (Dict)

{-|
-}
type alias CustomError
  = List Message

{-|
-}
noModel : CustomError
noModel =
  [ note "No model! Call Elmer.init to provide a model."
  ]


{-|
-}
wrongTitle : String -> String -> CustomError
wrongTitle expected actual =
  [ fact "Expected document to have title" expected
  , fact "but it has" actual
  ]


{-|
-}
noTitle : String -> CustomError
noTitle expected =
  [ fact "Expected document to have title" expected
  , note "but the supplied view function does not result in a Document value"
  ]


{-|
-}
noLocation : String -> CustomError
noLocation expected =
  [ fact "Expected to be at location:" expected
  , note "but no location has been set"
  ]


{-|
-}
wrongLocation : String -> String -> CustomError
wrongLocation expected actual =
  [ fact "Expected to be at location:" expected
  , fact "but location is:" actual
  ]


{-|
-}
sendUrlRequiresApplication : CustomError
sendUrlRequiresApplication =
  [ note "sendUrlRequest can only be used when testing an Elm Html application."
  , note "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
badUrl : String -> String -> CustomError
badUrl fun expected =
  [ fact ("Fake " ++ fun ++ " could not process url") expected
  , note "because it does not appear to be a url"
  ]


{-|
-}
navigationSpyRequiresApplication : String -> String -> CustomError
navigationSpyRequiresApplication fun expected =
  [ fact ("Fake " ++ fun ++ " could not process url") expected
  , note "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
elementNotFound : String -> String -> CustomError
elementNotFound selector dom =
  [ fact "The targeted html element was not found" selector
  , fact "The current view is" dom
  ]


{-|
-}
wrongAttribute : String -> String -> String -> CustomError
wrongAttribute property expectedValue actualValue =
  [ fact "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , fact "but it has" <| property ++ " = " ++ actualValue
  ]


{-|
-}
noAttribute : String -> String -> CustomError
noAttribute property expectedValue =
  [ fact "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , note "but it has no attribute with that name"
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
  [ fact "Expected element to have attribute" <| property ++ " = " ++ expectedValue
  , fact "but it has these attributes" messages ]


{-|
-}
noRequest : String -> CustomError
noRequest expectedRoute =
  [ fact "Expected request for" expectedRoute
  , note "but no requests have been made"
  ]


{-|
-}
wrongRequest : String -> String -> CustomError
wrongRequest expectedRoute actualRequests =
  [ fact "Expected request for" expectedRoute
  , fact "but only found these requests" actualRequests
  ]

{-|
-}
requestMatcherFailed : String -> String -> CustomError
requestMatcherFailed expectedRoute failure =
  [ fact "Requests matching" expectedRoute
  , note "failed to meet the expectations:"
  , note failure
  ]

{-|
-}
failedToActivateSpies : String -> CustomError
failedToActivateSpies errors =
  [ fact "Failed to activate one or more spies" errors
  ]

{-|
-}
unknownSpy : String -> CustomError
unknownSpy spyName =
   [ fact "Attempted to make expectations about calls to" spyName
   , note "but that function has no spy registered for it."
   , note "Check Elmer.Spy.use to make sure you've registered the spies you need."
   ]

{-|
-}
wrongNumberOfSpyCalls : String -> Int -> Int -> CustomError
wrongNumberOfSpyCalls name expectedCallCount actualCallCount =
  [ fact ("Expected spy " ++ name ++ " to have been called") <| timesString expectedCallCount
  , fact "but it was called" <| timesString actualCallCount
  ]


timesString : Int -> String
timesString times =
  if times == 1 then
    (String.fromInt times) ++ " time"
  else
    (String.fromInt times) ++ " times"


{-|
-}
badSpyIdentifier : CustomError
badSpyIdentifier =
  [ note "The function referenced by Elmer.Spy.expect could not be identified."
  ]


{-|
-}
eventHandlerNotFound : String -> String -> CustomError
eventHandlerNotFound actualEvents selector =
  [ fact "These events were triggered" actualEvents
  , fact "But there were no relevant event handlers on the targeted element" selector
  ]


{-|
-}
noElementTargetedForEvent : CustomError
noElementTargetedForEvent =
  [ note "No element has been targeted. Use Elmer.Html.target to identify an element to receive the event."
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