module Elmer.Message exposing
  ( Message
  , note
  , fact
  , format
  )

{-| Functions for producing messages that explain why a test has failed.

Note that these functions are mainly useful when writing extensions to Elmer
or custom matchers.

@docs Message, note, fact, format

-}

{-| Represents a message explaining why a test has failed.
-}
type Message =
  Message 
    { statement: String
    , detail: Maybe String
    }


{-| Produce a message that consists in one line of text.

-}
note : String -> Message
note desc =
  Message 
    { statement = desc
    , detail = Nothing
    }


{-| Produce a message that consists in a description of the failure
plus details about what exactly failed or why.

    [ fact "Expected" "something"
    , fact "to equal" "nothing"
    ]
      |> Message.format

will produce text that looks something like:

    Expected
      
      something
    
    to equal
    
      nothing

-}
fact : String -> String -> Message
fact stmt detail =
  Message
    { statement = stmt
    , detail = Just detail
    }


{-| Produce a string from a list of messages.

    [ fact "Expected" "something"
    , fact "to equal" "nothing"
    , note "but it does not."
    ]
      |> Message.format

will produce text that looks something like:

    Expected
      
      something
    
    to equal
    
      nothing

    but it does not.

-}
format : List Message -> String
format messages =
  List.map formatMessage messages
    |> joinMessages


formatMessage : Message -> String
formatMessage (Message msg) =
  case msg.detail of
    Just detail ->
      msg.statement ++ "\n\n" ++ (formatDetail detail)
    Nothing ->
      msg.statement


formatDetail : String -> String
formatDetail detail =
  String.split "\n" detail
    |> List.foldl (\s msg -> msg ++ "\t" ++ s ++ "\n") ""
    |> String.trimRight


joinMessages : List String -> String
joinMessages =
  String.join "\n\n"