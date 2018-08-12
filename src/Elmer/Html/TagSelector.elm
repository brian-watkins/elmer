module Elmer.Html.TagSelector exposing 
    ( TagSelector
    , from
    , empty
    )

import Parser exposing (Parser, (|.), (|=))
import Set

type alias TagSelector =
    { tag : Maybe String
    , characteristicName : Maybe String
    , characteristicValue : Maybe String
    , class : Maybe String
    }


empty : TagSelector
empty =
    { tag = Nothing
    , characteristicName = Nothing
    , characteristicValue = Nothing
    , class = Nothing
    }


from : String -> TagSelector
from selector =
    Parser.run parser selector
        |> Result.withDefault empty


parser : Parser TagSelector
parser =
    Parser.succeed TagSelector
        |= Parser.oneOf 
            [ Parser.map Just tagVariable
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.map Just attributeName 
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.map Just attributeValue
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.map Just classVariable
            , Parser.succeed Nothing
            ]

tagVariable : Parser String
tagVariable =
    Parser.variable
        { start = \c -> c /= '['
        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
        , reserved = Set.empty
        }

classVariable : Parser String
classVariable =
    Parser.succeed identity
        |. Parser.oneOf
            [ Parser.symbol "."
            , Parser.symbol "]."
            ]
        |= variableName

attributeName : Parser String
attributeName =
    Parser.succeed identity
        |. Parser.symbol "["
        |= variableName

attributeValue : Parser String
attributeValue =
    Parser.succeed identity
        |. Parser.symbol "='"
        |= variableName
        |. Parser.symbol "']"

variableName : Parser String
variableName =
    Parser.variable
        { start = \_ -> True  
        , inner = \c -> Char.isAlphaNum c || c == '-' || c == '_'
        , reserved = Set.empty
        }