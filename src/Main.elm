module Main exposing (main)

import Browser
import Browser.Events as BE
import Char exposing (Char)
import Helpers exposing (NonEmptyList)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Random
import String
import Task
import Time


type alias Model =
    { words : NonEmptyList String
    , currWord : String
    , typingBuf : String
    , startTime : Time.Posix
    , lastWordSpeed : Int
    , typingResult : TypingResult
    , wpmGoal : Int
    , successiveAchieved : Int
    }


type TypingResult
    = NotStarted
    | InProgress
    | OK
    | TypingMistake
    | TooSlow


type Msg
    = ChangeWord
    | SetGoal Int
    | GotNextword String
    | KeyPressed (Maybe Char)
    | StartTimer Time.Posix
    | StopTimer Time.Posix
    | Reset
    | GetWordListResp (Result Http.Error (List String))


init : ( Model, Cmd Msg )
init =
    ( { words = ( "", [] )
      , currWord = "loading..."
      , typingBuf = ""
      , startTime = Time.millisToPosix 0
      , lastWordSpeed = 0
      , typingResult = NotStarted
      , wpmGoal = 90
      , successiveAchieved = 0
      }
    , getWords
    )


countToNext : Int
countToNext =
    3


getWords : Cmd Msg
getWords =
    Http.get
        { url = "https://raw.githubusercontent.com/dariusk/corpora/master/data/words/common.json"
        , expect = Http.expectJson GetWordListResp (D.field "commonWords" (D.list D.string))
        }


askNewWord : NonEmptyList String -> Cmd Msg
askNewWord l =
    Random.generate GotNextword <| Helpers.choose l


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        SetGoal speed ->
            if speed < 10 then
                ( m, Cmd.none )

            else
                ( { m | wpmGoal = speed }, Cmd.none )

        GetWordListResp resp ->
            case resp of
                Ok list ->
                    case List.filter (\w -> String.length w > 1) list of
                        x :: xs ->
                            ( { m | words = ( x, xs ) }, askNewWord ( x, xs ) )

                        _ ->
                            ( { m | currWord = "", words = ( "", [] ) }, Cmd.none )

                Err _ ->
                    ( m, Cmd.none )

        Reset ->
            ( { m | typingBuf = "", successiveAchieved = 0, typingResult = NotStarted }, Cmd.none )

        ChangeWord ->
            ( m, askNewWord m.words )

        GotNextword w ->
            ( { m | currWord = w, typingBuf = "", successiveAchieved = 0, typingResult = NotStarted }, Cmd.none )

        StartTimer t ->
            ( { m | startTime = t, typingResult = InProgress }, Cmd.none )

        StopTimer t ->
            let
                ( typingResult_, lastWordSpeed_ ) =
                    let
                        speed =
                            Helpers.wpm m.currWord (Time.posixToMillis t - Time.posixToMillis m.startTime)
                    in
                    if Helpers.checkWord m.currWord m.typingBuf then
                        if speed >= m.wpmGoal then
                            ( OK, speed )

                        else
                            ( TooSlow, speed )

                    else
                        ( TypingMistake, 0 )

                successiveAchieved_ =
                    if typingResult_ == OK then
                        m.successiveAchieved + 1

                    else
                        0

                cmd =
                    if typingResult_ == OK && successiveAchieved_ >= countToNext then
                        askNewWord m.words

                    else
                        Cmd.none
            in
            ( { m
                | lastWordSpeed = lastWordSpeed_
                , successiveAchieved = successiveAchieved_
                , typingBuf = ""
                , typingResult = typingResult_
              }
            , cmd
            )

        KeyPressed k ->
            case k of
                Just char ->
                    let
                        typingBuf_ =
                            String.cons char m.typingBuf

                        ( isWordBeginning, isWordEnd ) =
                            ( m.typingBuf == "", String.length typingBuf_ == String.length m.currWord )

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


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map toKey (D.field "key" D.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            KeyPressed (Just char)

        _ ->
            if string == "Escape" then
                Reset

            else
                KeyPressed Nothing


playingView : String -> String -> List (Html Msg)
playingView tB cW =
    [ div [ HA.class "row", HA.style "text-align" "center" ]
        [ div [ HA.class "column" ]
            [ Html.h1 [] [ text cW ]
            , Html.h1 [] [ text <| typingProgress (String.length tB) (String.length cW) ]
            ]
        ]
    ]


statusBar : TypingResult -> Html a
statusBar tR =
    div []
        [ text <|
            case tR of
                InProgress ->
                    "..."

                OK ->
                    "ok"

                TooSlow ->
                    "too slow"

                TypingMistake ->
                    "you made a typo"

                NotStarted ->
                    "ready ?"
        ]


typingProgress : Int -> Int -> String
typingProgress typed goal =
    String.repeat typed "*" ++ String.repeat (goal - typed) "_"


view : Model -> Html Msg
view m =
    div [ HA.class "container" ] <|
        playingView m.typingBuf m.currWord
            ++ [ div [ HA.class "row" ]
                    [ div [ HA.class "column column-50 column-offset-25" ]
                        [ div []
                            [ text <| "speed goal : " ++ String.fromInt m.wpmGoal
                            , text <| ", last speed : " ++ (String.fromInt <| m.lastWordSpeed) ++ " wpm"
                            ]
                        , div [] [ text <| "achieved before next word : " ++ String.fromInt m.successiveAchieved ++ " / 3" ]
                        , statusBar m.typingResult
                        ]
                    ]
               , div [ HA.class "row", HA.style "margin-top" "30px" ]
                    [ div [ HA.class "column" ] []
                    , div [ HA.class "column" ] [ button [ HA.class "column", HE.onClick (SetGoal <| m.wpmGoal - 10), HA.class "button button-outline" ] [ text "--" ] ]
                    , div [ HA.class "column" ] [ button [ HA.class "column", HE.onClick ChangeWord, HA.class "button button-outline" ] [ text "skip" ] ]
                    , div [ HA.class "column" ] [ button [ HA.class "column", HE.onClick (SetGoal <| m.wpmGoal + 10), HA.class "button button-outline" ] [ text "++" ] ]
                    , div [ HA.class "column" ] []
                    ]
               , div [ HA.style "position" "fixed", HA.style "bottom" "0" ]
                    [ text "Tip: use the Esc key to reset your current word"
                    ]
               ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    BE.onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
