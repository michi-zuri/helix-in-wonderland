module Chapter.Story exposing
    ( Story, begin, tell, conclude
    , Heard(..), hear, tick
    , isPlaying, isTalking, view
    , instructions
    )

{-| The rhythm every chapter shares: an opening story, then play, then a
closing story. Short conversations can also happen during play (Alice finds
a key, say).

A chapter keeps one `Story` in its model and asks it first about every key:

    case Story.hear texts key model.story of
        Story.NotTalking ->
            -- no dialog is open: the key is for the game

        Story.Swallowed ->
            -- a dialog is open and the key was not Enter or space

        Story.Advanced story ->
            -- the dialog moved on

        Story.IntroOver story ->
            -- the opening story is finished: play starts

        Story.OutroOver story ->
            -- the closing story is finished: report the score

@docs Story, begin, tell, conclude
@docs Heard, hear, tick
@docs isPlaying, isTalking, view
@docs instructions

-}

import Chapter.Contract as Contract
import Html exposing (Html)
import Texts exposing (Texts)
import Ui.Dialog as Dialog exposing (Dialog, Line)


type Story
    = Story
        { phase : Phase
        , dialog : Maybe Dialog
        }


type Phase
    = Intro
    | Playing
    | Outro
    | Over


{-| Start a chapter with its opening story.
-}
begin : Line -> List Line -> Story
begin first rest =
    Story { phase = Intro, dialog = Just (Dialog.start first rest) }


{-| A conversation in the middle of play. Play continues when it ends.
-}
tell : Line -> List Line -> Story -> Story
tell first rest (Story s) =
    Story { s | dialog = Just (Dialog.start first rest) }


{-| The closing story, replacing whatever was going on. When it has been
read, `hear` returns `OutroOver`.
-}
conclude : Line -> List Line -> Story
conclude first rest =
    Story { phase = Outro, dialog = Just (Dialog.start first rest) }


type Heard
    = NotTalking
    | Swallowed
    | Advanced Story
    | IntroOver Story
    | OutroOver Story


{-| Enter and space move a dialog on; other keys are swallowed while
someone is talking, so nothing happens in the game behind the dialog box.
-}
hear : Texts -> String -> Story -> Heard
hear texts key (Story s) =
    case s.dialog of
        Nothing ->
            NotTalking

        Just dialog ->
            if key /= "Enter" && key /= " " then
                Swallowed

            else
                case Dialog.advance texts dialog of
                    Just next ->
                        Advanced (Story { s | dialog = Just next })

                    Nothing ->
                        case s.phase of
                            Intro ->
                                IntroOver (Story { phase = Playing, dialog = Nothing })

                            Outro ->
                                OutroOver (Story { phase = Over, dialog = Nothing })

                            _ ->
                                Advanced (Story { s | dialog = Nothing })


{-| Time passing: reveals the dialog text letter by letter.
-}
tick : Float -> Story -> Story
tick milliseconds (Story s) =
    Story { s | dialog = Maybe.map (Dialog.tick milliseconds) s.dialog }


{-| True when the player is in control: play has started, nobody is talking
and the chapter is not over.
-}
isPlaying : Story -> Bool
isPlaying ((Story s) as story) =
    s.phase == Playing && not (isTalking story)


isTalking : Story -> Bool
isTalking (Story s) =
    case s.dialog of
        Just _ ->
            True

        Nothing ->
            False


{-| The dialog box, if someone is talking.
-}
view : Texts -> Story -> Html msg
view texts (Story s) =
    case s.dialog of
        Just dialog ->
            Dialog.view texts dialog

        Nothing ->
            Html.text ""


{-| Instructions wait until they are relevant. While the opening story
plays nothing can be typed yet, so a chapter's instructions would only be
read too early and forgotten: the panel below the stage stays empty. Every
chapter's `view` goes through this:

    view context model =
        Story.instructions model.story (scene context model)

-}
instructions : Story -> Contract.View msg -> Contract.View msg
instructions (Story s) chapterView =
    case s.phase of
        Intro ->
            { chapterView | panel = Html.text "" }

        _ ->
            chapterView
