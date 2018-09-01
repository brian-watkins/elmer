module Elmer.Errors exposing
  ( noModel
  , noTitle
  , wrongTitle
  , noLocation
  , wrongLocation
  , sendUrlRequiresApplication
  , badUrl
  , navigationSpyRequiresApplication
  , elementNotFound
  )

{-| Exposed for testing

@docs noModel, noTitle, wrongTitle, noLocation, wrongLocation, sendUrlRequiresApplication
@docs badUrl, navigationSpyRequiresApplication, elementNotFound

-}

import Elmer.Printer exposing (..)

{-|
-}
noModel : String
noModel =
  "No model! Call Elmer.init to provide a model."

{-|
-}
wrongTitle : String -> String -> String
wrongTitle expected actual =
  format 
  [ message "Expected document to have title" expected
  , message "but it has" actual
  ]


{-|
-}
noTitle : String -> String
noTitle expected =
  format
  [ message "Expected document to have title" expected
  , description "but the supplied view function does not result in a Document value"
  ]

{-|
-}
noLocation : String -> String
noLocation expected =
  format
  [ message "Expected to be at location:" expected
  , description "but no location has been set"
  ]

{-|
-}
wrongLocation : String -> String -> String
wrongLocation expected actual =
  format
  [ message "Expected to be at location:" expected
  , message "but location is:" actual
  ]


{-|
-}
sendUrlRequiresApplication : String
sendUrlRequiresApplication =
  format
  [ description "sendUrlRequest can only be used when testing an Elm Html application."
  , description "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
badUrl : String -> String -> String
badUrl fun expected =
  format
  [ message ("Fake " ++ fun ++ " could not process url") expected
  , description "because it does not appear to be a url"
  ]

{-|
-}
navigationSpyRequiresApplication : String -> String -> String
navigationSpyRequiresApplication fun expected =
  format
  [ message ("Fake " ++ fun ++ " could not process url") expected
  , description "Use Elmer.Application.given to initialize this test."
  ]


{-|
-}
elementNotFound : String -> String -> String
elementNotFound selector dom =
  format
  [ message "No html element found with selector" selector
  , message "The current view is" dom
  ]
