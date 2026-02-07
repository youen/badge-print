port module Main exposing (..)

import Browser
import Data.Badge as Badge exposing (Badge)
import File exposing (File)
import Html exposing (Html, button, div, h1, input, option, select, text, textarea)
import Html.Attributes exposing (accept, class, placeholder, rows, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as D
import Task
import Util.Parser as Parser


port print : () -> Cmd msg



-- MODEL


type BadgeSize
    = Standard -- 85x55mm
    | Large -- 90x60mm
    | A6 -- 105x148mm


type Orientation
    = Landscape
    | Portrait


type alias Model =
    { badges : List Badge
    , size : BadgeSize
    , orientation : Orientation
    , logo : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { badges = [], size = Standard, orientation = Landscape, logo = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = UpdateNames String
    | GotLogo File
    | LogoRead String
    | RequestPrint
    | SetSize String
    | ToggleOrientation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleOrientation ->
            let
                newOrientation =
                    case model.orientation of
                        Landscape ->
                            Portrait

                        Portrait ->
                            Landscape
            in
            ( { model | orientation = newOrientation }, Cmd.none )

        SetSize str ->
            let
                newSize =
                    case str of
                        "Standard" ->
                            Standard

                        "Large" ->
                            Large

                        "A6" ->
                            A6

                        _ ->
                            Standard
            in
            ( { model | size = newSize }, Cmd.none )

        RequestPrint ->
            ( model, print () )

        UpdateNames input ->
            let
                parsedNames =
                    Parser.parseNames input

                newBadges =
                    List.map (\( first, last ) -> Badge.create first last model.logo) parsedNames
            in
            ( { model | badges = newBadges }
            , Cmd.none
            )

        GotLogo file ->
            ( model
            , Task.perform LogoRead (File.toUrl file)
            )

        LogoRead content ->
            let
                updateBadgeLogo b =
                    { b | logo = Just content }

                newBadges =
                    List.map updateBadgeLogo model.badges
            in
            ( { model | badges = newBadges, logo = Just content }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "p-8 print:p-0" ]
        [ div [ class "print:hidden" ]
            [ h1 [ class "text-3xl font-bold mb-6" ] [ text "Générateur de Badges" ]
            , div [ class "mb-6" ]
                [ div [ class "mb-2 font-bold" ] [ text "1. Choisissez un logo" ]
                , input
                    [ type_ "file"
                    , accept "image/*"
                    , on "change" (D.map GotLogo (D.at [ "target", "files", "0" ] File.decoder))
                    ]
                    []
                ]
            , div [ class "mb-6" ]
                [ textarea
                    [ onInput UpdateNames
                    , class "w-full p-2 border rounded"
                    , placeholder "Collez la liste des noms ici (un par ligne, ex: Prénom Nom)"
                    , rows 5
                    ]
                    []
                ]
            , div [ class "mb-6" ]
                [ div [ class "mb-2 font-bold" ] [ text "3. Options d'impression" ]
                , div [ class "flex gap-4 items-center mb-4" ]
                    [ text "Taille du badge :"
                    , select
                        [ onInput SetSize
                        , class "p-2 border rounded"
                        , value (sizeToString model.size)
                        ]
                        [ option [ value "Standard" ] [ text "Standard (85x55mm)" ]
                        , option [ value "Large" ] [ text "Large (90x60mm)" ]
                        , option [ value "A6" ] [ text "A6 (105x148mm)" ]
                        ]
                    ]
                , div [ class "flex gap-4 items-center mb-4" ]
                    [ text "Orientation :"
                    , button
                        [ onClick ToggleOrientation
                        , class "bg-gray-200 hover:bg-gray-300 text-gray-800 font-bold py-2 px-4 rounded"
                        ]
                        [ text
                            (case model.orientation of
                                Landscape ->
                                    "Paysage ↔"

                                Portrait ->
                                    "Portrait ↕"
                            )
                        ]
                    ]
                , div [ class "flex gap-4" ]
                    [ button
                        [ onClick RequestPrint
                        , class "bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded"
                        ]
                        [ text "🖨 Imprimer" ]
                    ]
                ]
            ]
        , div
            [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 print:grid-cols-2 print:gap-0 print:w-[210mm] print:mx-auto" ]
            (List.map (viewBadge model.size model.orientation) model.badges)
        ]


sizeToString : BadgeSize -> String
sizeToString size =
    case size of
        Standard ->
            "Standard"

        Large ->
            "Large"

        A6 ->
            "A6"


viewBadge : BadgeSize -> Orientation -> Badge -> Html Msg
viewBadge size orientation badge =
    let
        ( w, h ) =
            case size of
                Standard ->
                    ( "85mm", "55mm" )

                Large ->
                    ( "90mm", "60mm" )

                A6 ->
                    ( "105mm", "148mm" )

        ( width, height ) =
            case orientation of
                Landscape ->
                    ( w, h )

                Portrait ->
                    ( h, w )
    in
    div
        [ class "relative bg-white border border-gray-300 shadow-md flex flex-col items-center justify-center text-center overflow-hidden break-inside-avoid print:border-none print:shadow-none"
        , Html.Attributes.style "width" width
        , Html.Attributes.style "height" height
        ]
        [ case badge.logo of
            Just src ->
                div [ class "absolute inset-0 opacity-10 flex items-center justify-center p-4" ]
                    [ Html.img [ Html.Attributes.src src, class "w-full h-full object-contain" ] [] ]

            Nothing ->
                text ""
        , div [ class "z-10 relative px-4" ]
            [ div [ class "font-bold text-2xl mb-1 leading-tight" ] [ text badge.firstName ]
            , div [ class "text-xl text-gray-700 uppercase tracking-widest" ] [ text badge.lastName ]
            ]
        , cropMarks
        ]


cropMarks : Html msg
cropMarks =
    div [ class "absolute inset-0 pointer-events-none print:block hidden" ]
        [ -- Top Left
          div [ class "absolute top-0 left-0 w-2 h-2 border-t border-l border-gray-400" ] []

        -- Top Right
        , div [ class "absolute top-0 right-0 w-2 h-2 border-t border-r border-gray-400" ] []

        -- Bottom Left
        , div [ class "absolute bottom-0 left-0 w-2 h-2 border-b border-l border-gray-400" ] []

        -- Bottom Right
        , div [ class "absolute bottom-0 right-0 w-2 h-2 border-b border-r border-gray-400" ] []
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model
        , subscriptions = \_ -> Sub.none
        }
