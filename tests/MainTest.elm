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
        , test "SetFontSize should update the font size in the model" <|
            \_ ->
                let
                    ( initialModel, _ ) =
                        init ()

                    ( newModel, _ ) =
                        update (SetFontSize "30") initialModel
                in
                Expect.equal newModel.fontSize 30.0
        , test "SetLogoY should update the logo Y position in the model" <|
            \_ ->
                let
                    ( initialModel, _ ) =
                        init ()

                    ( newModel, _ ) =
                        update (SetLogoY "30") initialModel
                in
                Expect.equal newModel.logoY 30.0
        , describe "Print Grid Logic"
            [ test "Standard Portrait should have 3 columns" <|
                \_ ->
                    Main.printGridCols Main.Standard Main.Portrait
                        |> Expect.equal 3
            , test "Standard Landscape should have 2 columns" <|
                \_ ->
                    Main.printGridCols Main.Standard Main.Landscape
                        |> Expect.equal 2
            , test "A6 Portrait should have 1 column" <|
                \_ ->
                    Main.printGridCols Main.A6 Main.Portrait
                        |> Expect.equal 1
            ]
        ]
