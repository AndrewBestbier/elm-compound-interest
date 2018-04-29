module Main exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import Html exposing (Html, div, h1, h4, input, label, li, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, for, id, placeholder, src, type_)
import Html.Events exposing (onInput)


---- MODEL ----


type alias Model =
    { principle : Float, interest : Float, age : Float, deposit : Float, inflation : Float, increase : Float }


init : ( Model, Cmd Msg )
init =
    ( { principle = 0, interest = 0, age = 0, deposit = 0, inflation = 0, increase = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangePrinciple String
    | ChangeInterest String
    | ChangeAge String
    | ChangeDeposit String
    | ChangeInflation String
    | ChangeDepositIncrease String


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

        ChangeInflation inflation ->
            ( { model | inflation = Result.withDefault 0 (String.toFloat inflation) }, Cmd.none )

        ChangeDepositIncrease increase ->
            ( { model | increase = Result.withDefault 0 (String.toFloat increase) }, Cmd.none )



---- VIEW ----


calculate model age =
    let
        p =
            model.principle

        r =
            model.interest / 100

        x =
            model.increase / 100

        n =
            12

        t =
            toFloat age - model.age

        pmt =
            model.deposit

        deposits =
            p + pmt * 12 * t

        calculation =
            p * (1 + r / 12) ^ (n * t) + ((pmt * (1 + r / 12) * (-1 + (1 + r / 12) ^ n) * ((1 + r / 12) ^ (n * t) - (1 + x) ^ t)) / ((r / 12) * (-1 + (1 + r / 12) ^ n - x)))

        presentValue =
            calculation / ((1 + model.inflation / 100) ^ t)
    in
    { balance = formatMoney calculation
    , deposits = formatMoney deposits
    , interest = formatMoney (calculation - deposits)
    , interestPercentage = format usLocale ((calculation - deposits) / calculation * 100)
    , presentValue = formatMoney presentValue
    }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "p-5 text-white display-4" ] [ text "Compound Interest Calculator" ]
        , inputs
        , results model
        ]


inputs =
    div [ class "row" ]
        [ inputField "Starting Value (£)" ChangePrinciple "0"
        , inputField "Interest Rate (%)" ChangeInterest "If you are unsure try 7%"
        , inputField "Your Age" ChangeAge "0"
        , inputField "Monthly Deposit (£)" ChangeDeposit "0"
        , inputField "Inflation (%)" ChangeInflation "If you are unsure try 2.5%"
        , inputField "Yearly deposit increase (%)" ChangeDepositIncrease "0"
        ]


inputField title msg placeholderText =
    div [ class "col-md-4 mb-4" ]
        [ label [ class "text-white" ] [ text title ]
        , input [ class "form-control", placeholder placeholderText, type_ "number", onInput msg ] []
        ]


formatMoney value =
    "£" ++ format usLocale value


results model =
    if model.interest == 0 then
        h4 [ class "m-5 text-white" ] [ text "Awaiting input..." ]
    else
        div [] [ h1 [ class "m-5 text-white" ] [ text "Results:" ], resultsTable model ]


noResults =
    h4 [ class "m-5 text-white" ] [ text "Awaiting input..." ]


resultsTable model =
    let
        age =
            round model.age

        range =
            age :: (List.range (age + 1) 75 |> List.filter (\x -> x % 5 == 0))
    in
    div [ class "card mt-6" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table mb-0" ]
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
                            [ text "Present day value" ]
                        , th []
                            [ text "Balance" ]
                        ]
                    ]
                , tbody []
                    (List.map
                        (\x ->
                            let
                                calculation =
                                    calculate model x
                            in
                            tr []
                                [ th [] [ text (toString x) ]
                                , td [] [ text calculation.deposits ]
                                , td [] [ text calculation.interest ]
                                , td [] [ text (calculation.interestPercentage ++ "%") ]
                                , td [] [ text calculation.presentValue ]
                                , td [ class "font-weight-bold" ] [ text calculation.balance ]
                                ]
                        )
                        range
                    )
                ]
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
