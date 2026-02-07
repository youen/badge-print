module Data.Badge exposing (Badge, create)


type alias Badge =
    { firstName : String
    , lastName : String
    , logo : Maybe String
    }


create : String -> String -> Maybe String -> Badge
create =
    Badge
