module Main exposing (main)

import Browser
import Browser.Events as BE
import Char exposing (Char)
import Helpers
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import String
import Task
import Time


type alias Model =
    { nextWords : List String
    , currWord : Maybe String
    , typingBuf : String
    , startTime : Time.Posix
    , lastWordSpeed : Int
    , typingResult : TypingResult
    , wpmGoal : Int
    , successiveAchieved : Int
    , newWord : Bool
    }


type TypingResult
    = OK
    | TypingMistake
    | TooSlow
    | NotStarted
    | InProgress


getWords : Cmd Msg
getWords =
    Http.get
        { url = "https://raw.githubusercontent.com/dariusk/corpora/master/data/words/common.json"
        , expect = Http.expectJson GotWords (D.field "commonWords" (D.list D.string))
        }


countToNext : Int
countToNext =
    3


initGoal : Int
initGoal =
    90


init : ( Model, Cmd Msg )
init =
    ( { nextWords = []
      , currWord = Nothing
      , typingBuf = ""
      , startTime = Time.millisToPosix 0
      , lastWordSpeed = 0
      , typingResult = NotStarted
      , wpmGoal = initGoal
      , successiveAchieved = 0
      , newWord = True
      }
    , getWords
    )


type Msg
    = Nextword
    | KeyPressed (Maybe Char)
    | StartTimer Time.Posix
    | StopTimer Time.Posix
    | Reset
    | GotWords (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        GotWords resp ->
            case resp of
                Ok list ->
                    let
                        ( c, l ) =
                            Helpers.getHeadTail <| List.filter (\w -> String.length w > 1) list
                    in
                    ( { m | currWord = c, nextWords = l }, Cmd.none )

                Err _ ->
                    ( { m | currWord = Nothing, nextWords = [] }, Cmd.none )

        Reset ->
            ( { m | typingBuf = "", successiveAchieved = 0, typingResult = NotStarted }, Cmd.none )

        Nextword ->
            case m.nextWords of
                x :: xs ->
                    ( { m | currWord = Just x, nextWords = xs, typingBuf = "", successiveAchieved = 0 }, Cmd.none )

                [] ->
                    ( { m | currWord = Nothing }, Cmd.none )

        StartTimer t ->
            ( { m | startTime = t, typingResult = InProgress }, Cmd.none )

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
                                if lastWordSpeed_ >= m.wpmGoal then
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
                            String.cons char m.typingBuf

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
    let
        remaining =
            goal - typed
    in
    String.repeat typed "*" ++ String.repeat remaining "_"


view : Model -> Html Msg
view m =
    div [ HA.class "container", HA.style "margin-top" "15%" ] <|
        case m.currWord of
            Just w ->
                playingView m.typingBuf w
                    ++ [ div [ HA.class "row", HA.style "margin-top" "30px" ]
                            [ div [ HA.class "column column-50 column-offset-25" ]
                                [ div [] [ text <| "last speed : " ++ (String.fromInt <| m.lastWordSpeed) ++ " wpm" ]
                                , div [] [ text <| "achieved before next word : " ++ String.fromInt m.successiveAchieved ++ " / 3" ]
                                , statusBar m.typingResult
                                ]
                            ]
                       , div [ HA.class "row", HA.style "margin-top" "30px" ]
                            [ div [ HA.class "column", HA.style "text-align" "center" ]
                                [ button
                                    [ HE.onClick Nextword, HA.class "button button-outline" ]
                                    [ text "skip" ]
                                ]
                            ]
                       , div [ HA.style "position" "fixed", HA.style "bottom" "0" ] [ text "Tip: use the Esc key to reset your current word" ]
                       ]

            Nothing ->
                [ text "end" ]


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
