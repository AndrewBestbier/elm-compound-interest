module Main exposing (..)

import Html exposing (Html, div, h1, input, li, text, ul)
import Html.Attributes exposing (placeholder, src, type_)
import Html.Events exposing (onInput)


---- MODEL ----


type alias Model =
    { principle : Float, interest : Float, age : Float, deposit : Float }


init : ( Model, Cmd Msg )
init =
    ( { principle = 0, interest = 0, age = 0, deposit = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangePrinciple String
    | ChangeInterest String
    | ChangeAge String
    | ChangeDeposit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePrinciple principle ->
            ( { model | principle = Result.withDefault 0 (String.toFloat principle) }, Cmd.none )

        ChangeInterest interest ->
            ( { model | interest = Result.withDefault 0 (String.toFloat interest) }, Cmd.none )

        ChangeAge age ->
            ( { model | age = Result.withDefault 0 (String.toFloat age) }, Cmd.none )

        ChangeDeposit deposit ->
            ( { model | deposit = Result.withDefault 0 (String.toFloat deposit) }, Cmd.none )



---- VIEW ----


calculate model age =
    let
        p =
            model.principle

        r =
            model.interest / 100

        n =
            12

        t =
            toFloat age - model.age

        pmt =
            model.deposit

        calculation =
            (p * (1 + r / n) ^ (n * t)) + (pmt * (((1 + r / n) ^ (n * t) - 1) / (r / n)))
    in
    toString calculation


view : Model -> Html Msg
view model =
    let
        age =
            round model.age

        range =
            List.range age 65 |> List.filter (\x -> x % 5 == 0)
    in
    div []
        [ h1 [] [ text "Compound Interest" ]
        , input [ placeholder "Principle", onInput ChangePrinciple, type_ "number" ] []
        , input [ placeholder "Interest Rate", onInput ChangeInterest, type_ "number" ] []
        , input [ placeholder "Your Age", onInput ChangeAge, type_ "number" ] []
        , input [ placeholder "Monthly Deposit", onInput ChangeDeposit, type_ "number" ] []
        , ul [] (List.map (\x -> li [] [ text (("At " ++ toString x) ++ " you will have £" ++ calculate model x) ]) range)
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
