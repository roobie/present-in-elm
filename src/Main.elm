module Main exposing (Msg(..), main, update, view)

import Browser
import Dict as D exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Parser as P exposing ((|.), (|=), Parser, Trailing(..))
import JsonParser


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { json : String, parseResult : Result (List P.DeadEnd) JsonParser.JsonObject }


parseJsonIntoElm model =
    Debug.log "T" { model | parseResult = JsonParser.run model.json }

init =
    parseJsonIntoElm
        { json = """{
    "primitives": [1, 2.0, true, false, null, "string"],
}"""
        , parseResult = Err []
        }


type Msg
    = Parse


update msg model =
    case msg of
        Parse ->
            parseJsonIntoElm model

view model =
    div []
        [ pre [] [ text model.json ]
        , showErrors model
        ]


showErrors : Model -> Html Msg
showErrors model =
    case model.parseResult of
        Ok _ ->
            div [] []

        Err err ->
            let
                msgs =
                    List.map deadEndToString err
            in
            div []
                (List.map (\msg -> div [] [ pre [] [ text msg ] ]) msgs)


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : P.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : P.Problem -> String
problemToString p =
    case p of
        P.Expecting s ->
            "expecting '" ++ s ++ "'"

        P.ExpectingInt ->
            "expecting int"

        P.ExpectingHex ->
            "expecting hex"

        P.ExpectingOctal ->
            "expecting octal"

        P.ExpectingBinary ->
            "expecting binary"

        P.ExpectingFloat ->
            "expecting float"

        P.ExpectingNumber ->
            "expecting number"

        P.ExpectingVariable ->
            "expecting variable"

        P.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        P.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        P.ExpectingEnd ->
            "expecting end"

        P.UnexpectedChar ->
            "unexpected char"

        P.Problem s ->
            "problem " ++ s

        P.BadRepeat ->
            "bad repeat"
