module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Events exposing (onClick)


initialWordList : List String
initialWordList =
    [ "aaa", "bbb" ]


type alias Model =
    { nextWords : List String
    , currWord : Maybe String
    }


init : Model
init =
    case initialWordList of
        x :: xs ->
            { nextWords = xs
            , currWord = Just x
            }

        [] ->
            { nextWords = []
            , currWord = Nothing
            }


type Msg
    = Nextword


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nextword ->
            case model.nextWords of
                x :: xs ->
                    { model | currWord = Just x, nextWords = xs }

                [] ->
                    { model | currWord = Nothing }


playingView : String -> List (Html.Html Msg)
playingView cW =
    [ div []
        [ button [ onClick Nextword ] [ text "next" ]
        ]
    , div []
        [ text cW
        ]
    ]


view : Model -> Html.Html Msg
view m =
    div [] <|
        case m.currWord of
            Just w ->
                playingView w

            Nothing ->
                [ text "end" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
