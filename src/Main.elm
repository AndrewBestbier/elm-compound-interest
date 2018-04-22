module Main exposing (..)

import Html exposing (Html, div, h1, input, text)
import Html.Attributes exposing (placeholder, src, type_)
import Html.Events exposing (onInput)


---- MODEL ----


type alias Model =
    { principle : Float, interest : Float, term : Float, deposit : Float }


init : ( Model, Cmd Msg )
init =
    ( { principle = 0, interest = 0, term = 0, deposit = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangePrinciple String
    | ChangeInterest String
    | ChangeTerm String
    | ChangeDeposit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePrinciple principle ->
            ( { model | principle = Result.withDefault 0 (String.toFloat principle) }, Cmd.none )

        ChangeInterest interest ->
            ( { model | interest = Result.withDefault 0 (String.toFloat interest) }, Cmd.none )

        ChangeTerm term ->
            ( { model | term = Result.withDefault 0 (String.toFloat term) }, Cmd.none )

        ChangeDeposit deposit ->
            ( { model | deposit = Result.withDefault 0 (String.toFloat deposit) }, Cmd.none )



---- VIEW ----


calculate model =
    let
        p =
            model.principle

        r =
            model.interest / 100

        n =
            12

        t =
            model.term

        pmt =
            model.deposit

        calculation =
            (p * (1 + r / n) ^ (n * t)) + (pmt * (((1 + r / n) ^ (n * t) - 1) / (r / n)))
    in
    toString calculation


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Compound Interest" ]
        , input [ placeholder "Principle", onInput ChangePrinciple, type_ "number" ] []
        , input [ placeholder "Interest Rate", onInput ChangeInterest, type_ "number" ] []
        , input [ placeholder "Term", onInput ChangeTerm, type_ "number" ] []
        , input [ placeholder "Monthly Deposit", onInput ChangeDeposit, type_ "number" ] []
        , h1 [] [ text ("Principle: " ++ toString model.principle) ]
        , h1 [] [ text ("Interest Rate: " ++ toString model.interest) ]
        , h1 [] [ text ("Term: " ++ toString model.term) ]
        , h1 [] [ text ("Monthly Deposit: " ++ toString model.deposit) ]
        , h1 [] [ text ("Final Amount: " ++ calculate model) ]
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
