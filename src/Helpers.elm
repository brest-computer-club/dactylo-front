module Helpers exposing (NonEmptyList, checkWord, choose, wpm)

import Random exposing (Generator)


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


type alias NonEmptyList a =
    ( a, List a )


get : Int -> NonEmptyList a -> a
get index ( c, list ) =
    let
        picked =
            list
                |> List.drop index
                |> List.head
    in
    case picked of
        Just p ->
            p

        Nothing ->
            c


choose : NonEmptyList a -> Generator a
choose ( h, list ) =
    Random.map
        (\i -> get i ( h, list ))
        (Random.int 0 <| List.length list - 1)
