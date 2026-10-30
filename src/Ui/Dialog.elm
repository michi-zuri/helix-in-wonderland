module Ui.Dialog exposing (Dialog, Line, advance, say, start, tick, view)

{-| The story box at the bottom of the stage, with a typewriter effect.

A line stores a _function_ that picks its text from `Texts`, not the text
itself. That way switching language in the middle of a conversation simply
re-renders the current line in the new language.

The first Enter shows the whole line at once; the next Enter moves on.

-}

import Html exposing (Html)
import Html.Attributes as A
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg
import Svg.Attributes as SA
import Texts exposing (Texts)


type alias Line =
    { speaker : Speaker
    , text : Texts -> String
    }


type Dialog
    = Dialog
        { current : Line
        , upcoming : List Line
        , revealedChars : Float
        }


charactersPerMillisecond : Float
charactersPerMillisecond =
    0.07


say : Speaker -> (Texts -> String) -> Line
say =
    Line


{-| A dialog always has at least one line, so it takes the first one apart.
-}
start : Line -> List Line -> Dialog
start first rest =
    Dialog { current = first, upcoming = rest, revealedChars = 0 }


tick : Float -> Dialog -> Dialog
tick milliseconds (Dialog d) =
    Dialog { d | revealedChars = d.revealedChars + milliseconds * charactersPerMillisecond }


{-| Returns `Nothing` when the last line has been read.
-}
advance : Texts -> Dialog -> Maybe Dialog
advance texts (Dialog d) =
    let
        fullLength =
            toFloat (String.length (d.current.text texts))
    in
    if d.revealedChars < fullLength then
        Just (Dialog { d | revealedChars = fullLength })

    else
        case d.upcoming of
            next :: rest ->
                Just (start next rest)

            [] ->
                Nothing


view : Texts -> Dialog -> Html msg
view texts (Dialog d) =
    Html.div [ A.class "dialog" ]
        [ portrait d.current.speaker
        , Html.div [ A.class "dialog-body" ]
            [ Html.span [ A.class "dialog-speaker" ] [ Html.text (texts.speaker d.current.speaker) ]
            , Html.p [ A.class "dialog-text" ]
                [ Html.text (String.left (floor d.revealedChars) (d.current.text texts)) ]
            , Html.span [ A.class "dialog-next" ] [ Html.text texts.continueHint ]
            ]
        ]


portrait : Speaker -> Html msg
portrait speaker =
    let
        sprite =
            case speaker of
                Narrator ->
                    Nothing

                Alice ->
                    Just Sprites.alice

                WhiteRabbit ->
                    Just Sprites.whiteRabbit

                Caterpillar ->
                    Just Sprites.caterpillar

                Mouse ->
                    Just Sprites.mouse

                CheshireCat ->
                    Just Sprites.cheshireCat
    in
    case sprite of
        Just s ->
            Svg.svg [ SA.class "dialog-portrait", SA.viewBox (centredSquare (Sprite.size s)) ] [ Sprite.view s ]

        Nothing ->
            Html.text ""


{-| A square viewBox with the sprite in its middle, so portraits of any
shape fill the same box.
-}
centredSquare : { width : Int, height : Int } -> String
centredSquare { width, height } =
    let
        side =
            max width height

        offset length =
            String.fromFloat (toFloat (length - side) / 2)
    in
    String.join " " [ offset width, offset height, String.fromInt side, String.fromInt side ]
