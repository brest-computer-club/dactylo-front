module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Char exposing (Char)
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Json.Decode
import String exposing (cons)
import Task
import Time


initWords : List String
initWords =
    [ "bonjour", "bbb" ]


type alias Model =
    { nextWords : List String
    , currWord : Maybe String
    , currTyped : String
    , startTime : Time.Posix
    , duration : Int
    , result : Maybe Bool
    }


init : ( Model, Cmd Msg )
init =
    case initWords of
        x :: xs ->
            ( { nextWords = xs
              , currWord = Just x
              , currTyped = ""
              , startTime = Time.millisToPosix 0
              , duration = 0
              , result = Nothing
              }
            , Cmd.none
            )

        [] ->
            ( { nextWords = []
              , currWord = Nothing
              , currTyped = ""
              , startTime = Time.millisToPosix 0
              , duration = 0
              , result = Nothing
              }
            , Cmd.none
            )


type Msg
    = Nextword
    | KeyPressed (Maybe Char)
    | StartTimer Time.Posix
    | StopTimer Time.Posix


checkWord : Maybe String -> String -> Bool
checkWord currWord currTyped =
    case currWord of
        Just w ->
            String.reverse w == currTyped

        Nothing ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Nextword ->
            case m.nextWords of
                x :: xs ->
                    ( { m | currWord = Just x, nextWords = xs, currTyped = "" }, Cmd.none )

                [] ->
                    ( { m | currWord = Nothing }, Cmd.none )

        StartTimer t ->
            ( { m | startTime = t, result = Nothing }, Cmd.none )

        StopTimer t ->
            ( { m | duration = Time.posixToMillis t - Time.posixToMillis m.startTime }, Cmd.none )

        KeyPressed k ->
            case k of
                Just char ->
                    let
                        currTyped_ =
                            cons char m.currTyped

                        ( isWordBeginning, isWordEnd ) =
                            case m.currWord of
                                Just curr ->
                                    ( m.currTyped == "", String.length currTyped_ == String.length curr )

                                Nothing ->
                                    ( False, False )

                        tasks =
                            if isWordBeginning then
                                Task.perform StartTimer Time.now

                            else if isWordEnd then
                                Task.perform StopTimer Time.now

                            else
                                Cmd.none

                        nextTyped =
                            if isWordEnd then
                                ""

                            else
                                currTyped_
                    in
                    ( { m
                        | currTyped = nextTyped
                        , result = Just (checkWord m.currWord currTyped_)
                      }
                    , tasks
                    )

                Nothing ->
                    ( m, Cmd.none )


playingView : String -> List (Html.Html Msg)
playingView cW =
    [ div []
        [ button [ onClick Nextword ] [ text "next" ]
        ]
    , div []
        [ text cW
        ]
    ]


wpm : String -> Int -> Int
wpm w dur =
    if dur /= 0 then
        floor <|
            (toFloat (String.length w) / 5)
                / (toFloat dur / (1000 * 60))

    else
        0


mayBoolStr : Maybe Bool -> String
mayBoolStr x =
    let
        boolStr : Bool -> String
        boolStr b =
            if b then
                "true"

            else
                "false"
    in
    case x of
        Just b ->
            boolStr b

        Nothing ->
            ""


view : Model -> Html.Html Msg
view m =
    div [] <|
        case m.currWord of
            Just w ->
                playingView w
                    ++ [ div [] [ text m.currTyped ]
                       , div [] [ text "===" ]
                       , div [] [ text <| (String.fromInt <| wpm w m.duration) ++ " wpm" ]
                       , div [] [ text "===" ]
                       , div [] [ text <| mayBoolStr m.result ]
                       ]

            Nothing ->
                [ text "end" ]


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            KeyPressed (Just char)

        _ ->
            KeyPressed Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress keyDecoder


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
