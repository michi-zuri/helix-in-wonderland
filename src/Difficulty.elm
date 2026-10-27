module Difficulty exposing (Difficulty(..), all, name, tarts)

{-| How hard a chapter is played. Every chapter with difficulties offers the
same three, and they are worth the same: easy one tart, challenge two, mad
three. What each one means is up to the chapter (see the chapter's texts).
-}

import Texts exposing (Texts)


type Difficulty
    = Easy
    | Challenge
    | Mad


all : List Difficulty
all =
    [ Easy, Challenge, Mad ]


tarts : Difficulty -> Int
tarts difficulty =
    case difficulty of
        Easy ->
            1

        Challenge ->
            2

        Mad ->
            3


name : Texts -> Difficulty -> String
name texts difficulty =
    case difficulty of
        Easy ->
            texts.difficulty.easy

        Challenge ->
            texts.difficulty.challenge

        Mad ->
            texts.difficulty.mad
