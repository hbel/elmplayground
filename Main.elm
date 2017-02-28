module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


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
    | DoNot


cbIter : List Char -> Int -> Bool
cbIter str braces =
    if braces < 0 then
        False
    else
        case str of
            [] ->
                braces == 0

            head :: tail ->
                if head == '(' then
                    cbIter tail (braces + 1)
                else if head == ')' then
                    cbIter tail (braces - 1)
                else
                    cbIter tail braces


checkBraces : String -> Bool
checkBraces str =
    cbIter (String.toList str) 0


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Do ->
            ( { model | output = toString (checkBraces model.input) }, Cmd.none )

        Change str ->
            ( { model | input = str }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ Html.Events.onInput Change ] [ text model.input ]
        , span [ Html.Attributes.style [ ( "width", "50px" ) ] ] [ text "  " ]
        , button [ Html.Events.onClick Do ] [ text "Calculate" ]
        , div [] [ text model.output ]
        ]
