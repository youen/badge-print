module BadgeTest exposing (..)

import Data.Badge as Badge exposing (Badge)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Badge Module"
        [ test "should create a badge with first name, last name and optional logo" <|
            \_ ->
                let
                    badge =
                        Badge.create "John" "Doe" (Just "role") (Just "city") (Just "logo.png")
                in
                Expect.all
                    [ \b -> Expect.equal b.firstName "John"
                    , \b -> Expect.equal b.lastName "Doe"
                    , \b -> Expect.equal b.role (Just "role")
                    , \b -> Expect.equal b.city (Just "city")
                    , \b -> Expect.equal b.logo (Just "logo.png")
                    ]
                    badge
        ]
