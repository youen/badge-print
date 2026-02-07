module Util.Parser exposing (parseNames)


parseNames : String -> List ( String, String )
parseNames input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map parseLine


parseLine : String -> ( String, String )
parseLine line =
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
