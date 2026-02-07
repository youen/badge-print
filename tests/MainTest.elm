module MainTest exposing (..)

import Data.Badge exposing (Badge)
import Expect
import Main exposing (Model, Msg(..), init, update)
import Test exposing (..)


suite : Test
suite =
    describe "Main Module"
        [ test "UpdateNames should parse names and add badges" <|
            \_ ->
                let
                    ( initialModel, _ ) =
                        init ()

                    input =
                        "Alice Wonderland\nBob Builder"

                    ( newModel, _ ) =
                        update (UpdateNames input) initialModel
                in
                Expect.equal (List.length newModel.badges) 2
        ]
