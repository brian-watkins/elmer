module Elmer.Navigation.Location exposing (asLocation)

import Navigation
import String


asLocation : String -> Navigation.Location
asLocation url =
    let
        path =
            findPath url
    in
        { href = url
        , host = ""
        , hostname = ""
        , protocol = findProtocol url
        , origin = ""
        , port_ = ""
        , pathname = "/" ++ path
        , search = findQueryString path
        , hash = findHash path
        , username = ""
        , password = ""
        }


findQueryString : String -> String
findQueryString path =
    if String.contains "?" path then
        let
            afterQuestionMark =
                Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.tail (String.split "?" path))))
        in
            "?" ++ (Maybe.withDefault "" (List.head (String.split "#" afterQuestionMark)))
    else
        ""


findPath : String -> String
findPath url =
    let
        urlComponents =
            String.split "/" (dropProtocol url)
    in
        String.join "/" (Maybe.withDefault [] (List.tail urlComponents))


findHash : String -> String
findHash path =
    case List.head (List.drop 1 (String.split "#" path)) of
        Just hash ->
            "#" ++ hash

        Nothing ->
            ""


findProtocol : String -> String
findProtocol url =
    if String.contains "://" url then
        Maybe.withDefault "" (List.head (String.split "://" url)) ++ ":"
    else
        ""


dropProtocol : String -> String
dropProtocol url =
    if String.startsWith "http://" url then
        String.dropLeft 7 url
    else
        url
