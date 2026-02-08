port module Main exposing (..)

import Browser
import Data.Badge as Badge exposing (Badge)
import File exposing (File)
import Html exposing (Html, a, button, div, h1, input, label, option, select, text, textarea)
import Html.Attributes exposing (accept, checked, class, href, name, placeholder, rows, target, type_, value)
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
    , textY : Float -- 0 to 100 percentage
    , logoOpacity : Float -- 0 to 1 scaling to css opacity
    , textBackground : Bool
    , rawInput : String
    , delimiter : String
    , logoMargin : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { badges = []
      , size = Standard
      , orientation = Landscape
      , logo = Nothing
      , textY = 50.0 -- Default center
      , logoOpacity = 0.1 -- Default 10%
      , textBackground = False
      , rawInput = ""
      , delimiter = " " -- Default space
      , logoMargin = 16.0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateNames String
    | GotLogo File
    | LogoRead String
    | RequestPrint
    | SetSize String
    | ToggleOrientation
    | SetTextY String
    | SetLogoOpacity String
    | ToggleTextBackground
    | SetDelimiter String
    | SetLogoMargin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTextY str ->
            let
                val =
                    String.toFloat str |> Maybe.withDefault model.textY
            in
            ( { model | textY = val }, Cmd.none )

        SetLogoOpacity str ->
            let
                val =
                    String.toFloat str |> Maybe.withDefault model.logoOpacity
            in
            ( { model | logoOpacity = val }, Cmd.none )

        SetLogoMargin str ->
            let
                val =
                    String.toFloat str |> Maybe.withDefault model.logoMargin
            in
            ( { model | logoMargin = val }, Cmd.none )

        ToggleTextBackground ->
            ( { model | textBackground = not model.textBackground }, Cmd.none )

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
                    Parser.parseNames model.delimiter input

                newBadges =
                    List.map (\( first, last ) -> Badge.create first last model.logo) parsedNames
            in
            ( { model | badges = newBadges, rawInput = input }
            , Cmd.none
            )

        SetDelimiter newDelimiter ->
            let
                parsedNames =
                    Parser.parseNames newDelimiter model.rawInput

                newBadges =
                    List.map (\( first, last ) -> Badge.create first last model.logo) parsedNames
            in
            ( { model | delimiter = newDelimiter, badges = newBadges }
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
            [ div [ class "flex justify-between items-center mb-6" ]
                [ h1 [ class "text-3xl font-bold" ] [ text "Générateur de Badges" ]
                , a
                    [ href "https://github.com/youen/badge-print"
                    , target "_blank"
                    , class "flex items-center gap-2 text-gray-400 hover:text-gray-600 transition-colors text-sm"
                    ]
                    [ div [ class "w-5 h-5 flex items-center justify-center opacity-70" ]
                        [ -- Simple SVG icon for GitHub if possible, or just text
                          text "📁"
                        ]
                    , text "Voir sur GitHub"
                    ]
                ]
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
                [ div [ class "mb-2 font-bold" ] [ text "2. Liste des noms" ]
                , div [ class "flex gap-4 mb-2 items-center text-sm text-gray-600" ]
                    [ text "Séparateur :"
                    , label [ class "flex items-center gap-1 cursor-pointer" ]
                        [ input [ type_ "radio", name "delimiter", onClick (SetDelimiter " "), checked (model.delimiter == " ") ] []
                        , text "Espace"
                        ]
                    , label [ class "flex items-center gap-1 cursor-pointer" ]
                        [ input [ type_ "radio", name "delimiter", onClick (SetDelimiter ","), checked (model.delimiter == ",") ] []
                        , text "Virgule"
                        ]
                    , label [ class "flex items-center gap-1 cursor-pointer" ]
                        [ input [ type_ "radio", name "delimiter", onClick (SetDelimiter ";"), checked (model.delimiter == ";") ] []
                        , text "Point-virgule"
                        ]
                    ]
                , textarea
                    [ onInput UpdateNames
                    , class "w-full p-2 border rounded"
                    , placeholder
                        (if model.delimiter == " " then
                            "Collez la liste des noms ici (ex: Prénom Nom)"

                         else
                            "Collez la liste des noms ici (ex: Prénom, Nom)"
                        )
                    , rows 5
                    , value model.rawInput
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
                , div [ class "mb-6 border-t pt-4" ]
                    [ div [ class "mb-2 font-bold" ] [ text "4. Personnalisation" ]
                    , div [ class "mb-4" ]
                        [ div [ class "flex justify-between mb-1" ]
                            [ text "Position verticale du nom"
                            , text (String.fromFloat model.textY ++ "%")
                            ]
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "100"
                            , value (String.fromFloat model.textY)
                            , onInput SetTextY
                            , class "w-full"
                            ]
                            []
                        ]
                    , div [ class "mb-4" ]
                        [ div [ class "flex justify-between mb-1" ]
                            [ text "Opacité du logo"
                            , text (String.fromInt (round (model.logoOpacity * 100)) ++ "%")
                            ]
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "1"
                            , Html.Attributes.step "0.1"
                            , value (String.fromFloat model.logoOpacity)
                            , onInput SetLogoOpacity
                            , class "w-full"
                            ]
                            []
                        ]
                    , div [ class "mb-4" ]
                        [ div [ class "flex justify-between mb-1" ]
                            [ text "Marge du logo"
                            , text (String.fromFloat model.logoMargin ++ "px")
                            ]
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "50"
                            , value (String.fromFloat model.logoMargin)
                            , onInput SetLogoMargin
                            , class "w-full"
                            ]
                            []
                        ]
                    , div [ class "flex items-center gap-2 mb-4" ]
                        [ input
                            [ type_ "checkbox"
                            , Html.Attributes.checked model.textBackground
                            , onClick ToggleTextBackground
                            , class "w-4 h-4"
                            ]
                            []
                        , text "Fond blanc sous le nom"
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
            (List.map (viewBadge model) model.badges)
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


viewBadge : Model -> Badge -> Html Msg
viewBadge model badge =
    let
        size =
            model.size

        orientation =
            model.orientation

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

        textBgClass =
            if model.textBackground then
                "bg-white/95 shadow-md rounded-lg p-3 backdrop-blur-sm"

            else
                "w-full"
    in
    div
        [ class "relative bg-white border border-gray-300 shadow-md flex flex-col items-center justify-center text-center overflow-hidden break-inside-avoid print:border-none print:shadow-none"
        , Html.Attributes.style "width" width
        , Html.Attributes.style "height" height
        ]
        [ case badge.logo of
            Just src ->
                div
                    [ class "absolute inset-0 flex items-center justify-center"
                    , Html.Attributes.style "opacity" (String.fromFloat model.logoOpacity)
                    , Html.Attributes.style "padding" (String.fromFloat model.logoMargin ++ "px")
                    ]
                    [ Html.img [ Html.Attributes.src src, class "w-full h-full object-contain" ] [] ]

            Nothing ->
                text ""
        , div
            [ class ("z-10 relative flex flex-col items-center justify-center " ++ textBgClass)
            , Html.Attributes.style "top" (String.fromFloat (model.textY - 50) ++ "%")
            ]
            [ div [ class "font-bold text-2xl leading-tight" ] [ text badge.firstName ]
            , div [ class "font-bold text-2xl uppercase leading-tight" ] [ text badge.lastName ]
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
