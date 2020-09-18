module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Char exposing (Char)
import Helpers
import Html exposing (button, div, text)
import Html.Events exposing (onClick)
import Json.Decode
import String exposing (cons)
import Task
import Time


type alias Model =
    { nextWords : List String
    , currWord : Maybe String
    , typingBuf : String
    , startTime : Time.Posix
    , lastWordSpeed : Int
    , typingResult : TypingResult
    , goal : Int
    , successiveAchieved : Int
    , newWord : Bool
    }


type TypingResult
    = OK
    | TypingMistake
    | TooSlow
    | NotStarted


initWords : List String
initWords =
    [ "bonjour", "elodie" ]


countToNext : Int
countToNext =
    3


initGoal : Int
initGoal =
    90


init : ( Model, Cmd Msg )
init =
    case initWords of
        x :: xs ->
            ( { nextWords = xs
              , currWord = Just x
              , typingBuf = ""
              , startTime = Time.millisToPosix 0
              , lastWordSpeed = 0
              , typingResult = NotStarted
              , goal = initGoal
              , successiveAchieved = 0
              , newWord = True
              }
            , Cmd.none
            )

        [] ->
            ( { nextWords = []
              , currWord = Nothing
              , typingBuf = ""
              , startTime = Time.millisToPosix 0
              , lastWordSpeed = 0
              , typingResult = NotStarted
              , goal = initGoal
              , successiveAchieved = 0
              , newWord = True
              }
            , Cmd.none
            )


type Msg
    = Nextword
    | KeyPressed (Maybe Char)
    | StartTimer Time.Posix
    | StopTimer Time.Posix
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Reset ->
            ( { m | typingBuf = "", successiveAchieved = 0 }, Cmd.none )

        Nextword ->
            case m.nextWords of
                x :: xs ->
                    ( { m | currWord = Just x, nextWords = xs, typingBuf = "" }, Cmd.none )

                [] ->
                    ( { m | currWord = Nothing }, Cmd.none )

        StartTimer t ->
            ( { m | startTime = t, typingResult = NotStarted }, Cmd.none )

        StopTimer t ->
            let
                lastWordSpeed_ =
                    case m.currWord of
                        Just w ->
                            Helpers.wpm w (Time.posixToMillis t - Time.posixToMillis m.startTime)

                        Nothing ->
                            0

                typingResult_ =
                    case m.currWord of
                        Just w ->
                            if Helpers.checkWord w m.typingBuf then
                                if lastWordSpeed_ >= m.goal then
                                    OK

                                else
                                    TooSlow

                            else
                                TypingMistake

                        Nothing ->
                            NotStarted

                ( currWord_, nextWords_, newWord_ ) =
                    if typingResult_ == OK && m.successiveAchieved + 1 >= countToNext then
                        case m.nextWords of
                            x :: xs ->
                                ( Just x, xs, True )

                            [] ->
                                ( Nothing, [], False )

                    else
                        ( m.currWord, m.nextWords, False )

                successiveAchieved_ =
                    if typingResult_ == OK && not newWord_ then
                        m.successiveAchieved + 1

                    else
                        0
            in
            ( { m
                | lastWordSpeed = lastWordSpeed_
                , successiveAchieved = successiveAchieved_
                , currWord = currWord_
                , nextWords = nextWords_
                , newWord = newWord_
                , typingBuf = ""
                , typingResult = typingResult_
              }
            , Cmd.none
            )

        KeyPressed k ->
            case k of
                Just char ->
                    let
                        typingBuf_ =
                            cons char m.typingBuf

                        ( isWordBeginning, isWordEnd ) =
                            case m.currWord of
                                Just curr ->
                                    ( m.typingBuf == "", String.length typingBuf_ == String.length curr )

                                Nothing ->
                                    ( False, False )

                        tasks =
                            if isWordBeginning then
                                Task.perform StartTimer Time.now

                            else if isWordEnd then
                                Task.perform StopTimer Time.now

                            else
                                Cmd.none
                    in
                    ( { m | typingBuf = typingBuf_ }, tasks )

                Nothing ->
                    ( m, Cmd.none )


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


playingView : String -> Int -> List (Html.Html Msg)
playingView cW successiveAchieved =
    [ div []
        [ button [ onClick Nextword ] [ text "next" ]
        , button [ onClick Reset ] [ text "reset" ]
        , text <| String.fromInt successiveAchieved
        ]
    , div []
        [ text cW
        ]
    ]


statusBar : String -> TypingResult -> Bool -> Html.Html Msg
statusBar tb tR newW =
    div []
        [ text <|
            if tb /= "" then
                "**typing**"

            else if not newW then
                case tR of
                    OK ->
                        "ok"

                    TooSlow ->
                        "too slow"

                    TypingMistake ->
                        "you made a typo"

                    NotStarted ->
                        "ready to type?"

            else
                ""
        ]


view : Model -> Html.Html Msg
view m =
    div [] <|
        case m.currWord of
            Just w ->
                playingView w m.successiveAchieved
                    ++ [ div [] [ text "===" ]
                       , div [] [ text <| (String.fromInt <| m.lastWordSpeed) ++ " wpm" ]
                       , div []
                            [ text "===" ]
                       , statusBar
                            m.typingBuf
                            m.typingResult
                            m.newWord
                       ]

            Nothing ->
                [ text "end" ]


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
