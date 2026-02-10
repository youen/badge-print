module Util.Parser exposing (ParsedBadge, parseNames)


type alias ParsedBadge =
    { firstName : String
    , lastName : String
    , role : Maybe String
    , city : Maybe String
    }


parseNames : String -> String -> List ParsedBadge
parseNames delimiter input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map (parseLine delimiter)


parseLine : String -> String -> ParsedBadge
parseLine delimiter line =
    if delimiter == " " then
        let
            parts =
                String.words line
        in
        case parts of
            [] ->
                { firstName = "", lastName = "", role = Nothing, city = Nothing }

            [ firstName ] ->
                { firstName = firstName, lastName = "", role = Nothing, city = Nothing }

            firstName :: rest ->
                { firstName = firstName, lastName = String.join " " rest, role = Nothing, city = Nothing }

    else
        case String.split delimiter line of
            [] ->
                { firstName = "", lastName = "", role = Nothing, city = Nothing }

            [ firstName ] ->
                { firstName = String.trim firstName, lastName = "", role = Nothing, city = Nothing }

            firstName :: lastName :: rest ->
                let
                    role =
                        List.head rest |> Maybe.map String.trim

                    city =
                        List.drop 1 rest |> List.head |> Maybe.map String.trim
                in
                { firstName = String.trim firstName
                , lastName = String.trim lastName
                , role = role
                , city = city
                }
