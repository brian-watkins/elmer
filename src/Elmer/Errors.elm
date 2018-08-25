module Elmer.Errors exposing
  ( noModel
  , noTitle
  , wrongTitle
  )

{-| Exposed for testing

@docs noModel, noTitle, wrongTitle

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