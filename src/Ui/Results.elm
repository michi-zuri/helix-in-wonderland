module Ui.Results exposing (view)

{-| The card shown after a chapter: tarts, the lesson in one line, stats.

`onNext` is `Nothing` when the next chapter is still locked (no tart this
time); then playing again is the main button.

-}

import Chapter.Contract exposing (Score)
import Chapter.Id as ChapterId exposing (ChapterId)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Texts exposing (Texts)
import Ui.Tarts as Tarts


view :
    { texts : Texts
    , chapter : ChapterId
    , score : Score
    , onNext : Maybe msg
    , onReplay : msg
    , onMenu : msg
    }
    -> Html msg
view { texts, chapter, score, onNext, onReplay, onMenu } =
    let
        info =
            texts.chapter chapter

        button class msg label key =
            Html.button [ A.class class, E.onClick msg ] [ Html.text (label ++ " "), Html.kbd [] [ Html.text key ] ]
    in
    Html.div [ A.class "card" ]
        [ Html.span [ A.class "eyebrow" ] [ Html.text (String.fromInt (ChapterId.number chapter) ++ " · " ++ info.name) ]
        , Html.h2 []
            [ Html.text
                (if score.tarts == 0 then
                    texts.results.noTart

                 else
                    texts.results.title
                )
            ]
        , Tarts.view "large" score.tarts
        , Html.p [ A.class "lesson" ]
            [ Html.text
                (if score.tarts == 0 then
                    texts.results.noTartHint

                 else
                    info.lesson
                )
            ]
        , Html.div [ A.class "stats" ]
            (List.map (\stat -> Html.span [] [ Html.text (stat.label texts), Html.b [] [ Html.text (stat.value texts) ] ]) score.stats)
        , Html.div [ A.class "actions" ]
            (case onNext of
                Just next ->
                    [ button "primary" next texts.results.next "Enter"
                    , button "" onReplay texts.results.replay "r"
                    , button "" onMenu texts.results.menu "m"
                    ]

                Nothing ->
                    [ button "primary" onReplay texts.results.replay "Enter"
                    , button "" onMenu texts.results.menu "m"
                    ]
            )
        ]
