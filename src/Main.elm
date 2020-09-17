module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Events exposing (onClick)


initialWordList : List String
initialWordList =
    [ "aaa", "bbb" ]


type alias Model =
    { currWords : List String
    , currWord : String
    , end : Bool
    }


init : Model
init =
    case initialWordList of
        x :: xs ->
            { currWords = xs
            , currWord = x
            , end = False
            }

        _ ->
            { currWords = []
            , currWord = ""
            , end = True
            }


type Msg
    = Nextword


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nextword ->
            case model.currWords of
                x :: xs ->
                    { model | currWord = x, currWords = xs }

                _ ->
                    { model | end = True }


playingView : Model -> List (Html.Html Msg)
playingView m =
    [ div []
        [ button [ onClick Nextword ] [ text "next" ]
        ]
    , div []
        [ text m.currWord
        ]
    ]


view : Model -> Html.Html Msg
view m =
    div [] <|
        if m.end then
            [ text "end" ]

        else
            playingView m


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
