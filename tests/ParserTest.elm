module ParserTest exposing (..)

import Data.Badge exposing (Badge)
import Expect
import Test exposing (..)
import Util.Parser as Parser


suite : Test
suite =
    describe "Parser Module"
        [ test "should parse a single name" <|
            \_ ->
                let
                    input =
                        "John;Doe;Coach;Paris"

                    expected =
                        [ { firstName = "John", lastName = "Doe", role = Just "Coach", city = Just "Paris" } ]

                    parsed =
                        Parser.parseNames ";" input
                in
                Expect.equal parsed expected
        , test "should parse multiple lines with semicolon" <|
            \_ ->
                let
                    input =
                        "John;Doe;Coach;Paris\nJane;Smith;Chaperon;Lyon"

                    expected =
                        [ { firstName = "John", lastName = "Doe", role = Just "Coach", city = Just "Paris" }
                        , { firstName = "Jane", lastName = "Smith", role = Just "Chaperon", city = Just "Lyon" }
                        ]
                in
                Expect.equal (Parser.parseNames ";" input) expected
        , test "should parse space separated names as before (role and city will be Nothing)" <|
            \_ ->
                let
                    input =
                        "John Doe"

                    expected =
                        [ { firstName = "John", lastName = "Doe", role = Nothing, city = Nothing } ]
                in
                Expect.equal (Parser.parseNames " " input) expected
        ]
