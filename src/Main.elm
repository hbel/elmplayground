module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Tools exposing (checkBraces, fib, fac)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    }


init : ( Model, Cmd msg )
init =
    ( Model "" "", Cmd.none )


type Msg
    = Do
    | Change String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Do ->
            let
                x =
                    case String.toInt model.input of
                        Ok i ->
                            i

                        Err e ->
                            0
            in
                ( { model | output = toString (fib x) ++ "   " ++ toString (fac x) }, Cmd.none )

        Change str ->
            ( { model | input = str }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ Html.Events.onInput Change ] [ text model.input ]
        , span [ Html.Attributes.style [ ( "width", "50px" ) ] ] [ text "  " ]
        , button [ Html.Events.onClick Do ] [ text "Calculate" ]
        , div [] [ text model.output ]
        ]
