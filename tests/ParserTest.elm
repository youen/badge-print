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
                        "John Doe"

                    expected =
                        [ "John" ]

                    -- Simplification: just checking first name logic first or full structure?
                    -- wait, the plan said parseNames : String -> List (String, String) or List Badge?
                    -- Step 2 instructions: "transformact text en une liste de Badge"
                    -- But let's verify generic parsing first or direct to Badge?
                    -- Plan said: parseNames : String -> List (String, String)
                    -- Let's stick to that for the Utility, then Main maps it to Badges.
                    parsed =
                        Parser.parseNames " " input
                in
                Expect.equal parsed [ ( "John", "Doe" ) ]
        , test "should parse multiple lines" <|
            \_ ->
                let
                    input =
                        "John Doe\nJane Smith"
                in
                Expect.equal (Parser.parseNames " " input) [ ( "John", "Doe" ), ( "Jane", "Smith" ) ]
        ]
