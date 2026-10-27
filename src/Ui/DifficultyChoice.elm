module Ui.DifficultyChoice exposing (Choice, Outcome(..), press, start, view)

{-| The card for picking a difficulty at the start of a chapter.

`j` / `k` (or the arrow keys) move the highlight, Enter picks it, and 1-3
pick directly. Enter is ignored for a moment after the card appears, so a
child hurrying through the story doesn't pick by accident.

-}

import Difficulty exposing (Difficulty(..))
import Html exposing (Html)
import Html.Attributes as A
import Texts exposing (Texts)
import Ui.Tarts as Tarts


type alias Choice =
    { highlighted : Difficulty
    , shownAt : Float
    }


{-| The card appears at game time `now`, with easy highlighted.
-}
start : Float -> Choice
start now =
    { highlighted = Easy, shownAt = now }


type Outcome
    = Moved Choice
    | Picked Difficulty
    | Unchanged


guardMilliseconds : Float
guardMilliseconds =
    500


press : Float -> String -> Choice -> Outcome
press now key choice =
    let
        move step =
            Moved { choice | highlighted = shift step choice.highlighted }
    in
    case key of
        "j" ->
            move 1

        "ArrowDown" ->
            move 1

        "k" ->
            move -1

        "ArrowUp" ->
            move -1

        "1" ->
            Picked Easy

        "2" ->
            Picked Challenge

        "3" ->
            Picked Mad

        "Enter" ->
            if now - choice.shownAt >= guardMilliseconds then
                Picked choice.highlighted

            else
                Unchanged

        _ ->
            Unchanged


shift : Int -> Difficulty -> Difficulty
shift step difficulty =
    let
        index =
            Difficulty.all
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, d ) -> d == difficulty)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0
    in
    Difficulty.all
        |> List.drop (modBy (List.length Difficulty.all) (index + step))
        |> List.head
        |> Maybe.withDefault difficulty


{-| The card, laid over the stage. `info` says what a difficulty means in
this chapter.
-}
view : { texts : Texts, title : String, hint : String, info : Difficulty -> String } -> Choice -> Html msg
view { texts, title, hint, info } choice =
    Html.div [ A.class "overlay" ]
        [ Html.div [ A.class "card" ]
            [ Html.h2 [] [ Html.text title ]
            , Html.ol [ A.class "chapters" ]
                (List.indexedMap
                    (\index difficulty ->
                        Html.li [ A.classList [ ( "selected", difficulty == choice.highlighted ) ] ]
                            [ Html.span [ A.class "chapter-number" ] [ Html.text (String.fromInt (index + 1)) ]
                            , Html.span [ A.class "chapter-text" ]
                                [ Html.span [ A.class "chapter-name" ] [ Html.text (Difficulty.name texts difficulty) ]
                                , Html.span [ A.class "chapter-skill" ] [ Html.text (info difficulty) ]
                                ]
                            , Tarts.view "small" (Difficulty.tarts difficulty)
                            ]
                    )
                    Difficulty.all
                )
            , Html.p [ A.class "hint" ] [ Html.text hint ]
            ]
        ]
