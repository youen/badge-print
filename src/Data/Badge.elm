module Data.Badge exposing (Badge, create)


type alias Badge =
    { firstName : String
    , lastName : String
    , role : Maybe String
    , city : Maybe String
    , logo : Maybe String
    }


create : String -> String -> Maybe String -> Maybe String -> Maybe String -> Badge
create =
    Badge
