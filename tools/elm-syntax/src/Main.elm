module Main exposing (..)

import Browser
import Elm.Interface exposing (Exposed(..))
import Elm.Parser
import Elm.Syntax.File as File
import Html exposing (Html, div, pre, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode as Encode
import Json.Print exposing (prettyString)


type alias Model =
    { src : String }


parse : String -> String
parse input =
    case Elm.Parser.parseToFile input of
        Err e ->
            "Failed: " ++ Debug.toString e

        Ok v ->
            "Success: " ++ (v |> File.encode |> valueToString |> prettyPrint)


valueToString : Json.Decode.Value -> String
valueToString value =
    Encode.encode 0 value


prettyPrint : String -> String
prettyPrint js =
    case js |> prettyString { indent = 2, columns = 100 } of
        Err e ->
            "Error: " ++ Debug.toString e

        Ok s ->
            s



--- INIT


init : Model
init =
    { src = "module Foo exposing (foo)\n" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change src ->
            { model | src = src }



-- VIEW


view : Model -> Html Msg
view { src } =
    div
        [ style "display" "flex"
        , style "height" "100vh"
        , style "width" "100%"
        , style "box-sizing" "border-box"
        , style "margin" "0"
        , style "overflow" "hidden"
        ]
        [ div
            [ style "width" "50%"
            , style "height" "100vh"
            , style "border-right" "1px solid #ccc"
            , style "box-sizing" "border-box"
            ]
            [ textarea
                [ placeholder "Elm code"
                , value src
                , onInput Change
                , style "width" "100%"
                , style "height" "100%"
                , style "resize" "none"
                , style "font-family" "monospace"
                , style "padding" "0.5rem"
                , style "box-sizing" "border-box"
                , style "border" "none"
                , style "margin" "0"
                ]
                []
            ]
        , div
            [ style "width" "50%"
            , style "height" "100vh"
            , style "box-sizing" "border-box"
            ]
            [ pre
                [ style "width" "100%"
                , style "height" "100%"
                , style "margin" "0"
                , style "padding" "0.5rem"
                , style "font-family" "monospace"
                , style "white-space" "pre-wrap"
                , style "overflow" "auto"
                , style "box-sizing" "border-box"
                ]
                [ Html.text (parse src) ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
