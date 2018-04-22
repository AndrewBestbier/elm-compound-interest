module Main exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import Html exposing (Html, div, h1, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, placeholder, src, type_)
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

        deposits =
            p + pmt * 12 * t
    in
    { balance = formatMoney calculation, deposits = formatMoney deposits, interest = formatMoney (calculation - deposits), interestPercentage = format usLocale ((calculation - deposits) / calculation * 100) }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text "Compound Interest" ]
        , inputs
        , h1 [ class "title" ] [ text "Results:" ]
        , results model
        ]


inputs =
    div [ class "columns is-mobile" ]
        [ div [ class "column is-three-fifths is-offset-one-fifth" ]
            [ inputField "Principle (£)" ChangePrinciple
            , inputField "Interest Rate (%)" ChangeInterest
            , inputField "Your Age" ChangeAge
            , inputField "Monthly Deposit (£)" ChangeDeposit
            ]
        ]


inputField title msg =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label" ]
            [ label [ class "label" ]
                [ text title ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ p [ class "control" ]
                    [ input [ class "input", placeholder "0", type_ "number", onInput msg ]
                        []
                    ]
                ]
            ]
        ]


formatMoney value =
    "£" ++ format usLocale value


results model =
    let
        age =
            round model.age

        range =
            List.range age 65 |> List.filter (\x -> x % 5 == 0)
    in
    div []
        [ table [ class "table is-fullwidth" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "Age" ]
                    , th []
                        [ text "Total Deposits" ]
                    , th []
                        [ text "Total Interest" ]
                    , th []
                        [ text "Interest Percentage" ]
                    , th []
                        [ text "Balance" ]
                    ]
                ]
            , tbody [] (List.map (\x -> tr [] [ th [] [ text (toString x) ], td [] [ text (calculate model x).deposits ], td [] [ text (calculate model x).interest ], td [] [ text ((calculate model x).interestPercentage ++ "%") ], td [] [ text (calculate model x).balance ] ]) range)
            ]
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
