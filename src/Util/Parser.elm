module Util.Parser exposing (parseNames)


parseNames : String -> String -> List ( String, String )
parseNames delimiter input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map (parseLine delimiter)


parseLine : String -> String -> ( String, String )
parseLine delimiter line =
    if delimiter == " " then
        let
            parts =
                String.words line
        in
        case parts of
            [] ->
                ( "", "" )

            [ firstName ] ->
                ( firstName, "" )

            firstName :: rest ->
                ( firstName, String.join " " rest )

    else
        case String.split delimiter line of
            [] ->
                ( "", "" )

            [ firstName ] ->
                ( String.trim firstName, "" )

            firstName :: rest ->
                ( String.trim firstName, String.join delimiter rest |> String.trim )
