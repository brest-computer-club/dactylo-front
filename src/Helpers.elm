module Helpers exposing (checkWord, getHeadTail, wpm)


checkWord : String -> String -> Bool
checkWord w currTyped =
    String.reverse w == currTyped


wpm : String -> Int -> Int
wpm w dur =
    if dur /= 0 then
        floor <|
            (toFloat (String.length w) / 5)
                / (toFloat dur / (1000 * 60))

    else
        0


getHeadTail : List String -> ( Maybe String, List String )
getHeadTail l =
    case l of
        x :: xs ->
            ( Just x, xs )

        [] ->
            ( Nothing, [] )
