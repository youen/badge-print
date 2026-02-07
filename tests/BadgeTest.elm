module BadgeTest exposing (..)

import Expect
import Test exposing (..)
import Data.Badge as Badge
import Data.Badge exposing (Badge)

suite : Test
suite =
    describe "Badge Module"
        [ test "should create a badge with first name, last name and optional logo" <|
            \_ ->
                let
                    badge =
                        Badge.create "John" "Doe" (Just "logo.png")
                in
                Expect.all
                    [ \b -> Expect.equal b.firstName "John"
                    , \b -> Expect.equal b.lastName "Doe"
                    , \b -> Expect.equal b.logo (Just "logo.png")
                    ]
                    badge
        ]
