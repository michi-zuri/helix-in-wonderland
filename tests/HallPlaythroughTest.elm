module HallPlaythroughTest exposing (suite)

{-| Play chapter 2 the way a child would: read the story, press keys, wait
for Alice to walk. No browser involved, just messages in, model and effects
out.
-}

import Chapter.Contract exposing (Context)
import Chapter.Hall as Hall
import Chapter.Story as Story
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Random
import Test exposing (Test, describe, test)
import Texts


context : Context
context =
    { texts = Texts.forLanguage German, layout = SwissQwertz, escapeKeys = Nothing }


press : String -> Run Hall.Model -> Run Hall.Model
press keys =
    Playthrough.sendAll (Hall.update context) (List.map (String.fromChar >> Hall.KeyPressed) (String.toList keys))


send : Hall.Msg -> Run Hall.Model -> Run Hall.Model
send =
    Playthrough.send (Hall.update context)


{-| Let time pass in 16 ms frames, long enough for any walk on this map.
-}
wait : Run Hall.Model -> Run Hall.Model
wait =
    Playthrough.sendAll (Hall.update context) (List.repeat 400 (Hall.Frame 16))


{-| Read every open dialog to the end (Enter reveals, Enter continues).
-}
readStory : Run Hall.Model -> Run Hall.Model
readStory =
    Playthrough.sendAll (Hall.update context) (List.repeat 12 (Hall.KeyPressed "Enter"))


start : Run Hall.Model
start =
    Playthrough.start (Hall.init (Random.initialSeed 1))


{-| Read the story and pick the easy hall with its number key.
-}
inEasyHall : Run Hall.Model
inEasyHall =
    start |> readStory |> press "1"


{-| How many dialog lines are left, for a dialog whose first line has had
time to show in full: Enter is pressed until the dialog closes, once for
that line, then twice per line (reveal, continue).
-}
countDialogLines : Layout -> Run Hall.Model -> Int
countDialogLines layout run =
    let
        go presses r =
            if not (Story.isTalking r.model.story) || presses > 40 then
                presses

            else
                go (presses + 1) (Playthrough.send (Hall.update { context | layout = layout }) (Hall.KeyPressed "Enter") r)
    in
    (go 0 run + 1) // 2


suite : Test
suite =
    describe "Chapter 2: The Hall of Doors"
        [ describe "no panel: the status line and dialogs say what is needed (#85)"
            [ test "while walking, the status line reminds of h j k l" <|
                \_ ->
                    inEasyHall
                        |> .model
                        |> Hall.view context
                        |> .status
                        |> .pendingKeys
                        |> Expect.equal "h← j↓ k↑ l→"
            , test "a count being typed takes its place, as in Helix" <|
                \_ ->
                    inEasyHall
                        |> press "5"
                        |> .model
                        |> Hall.view context
                        |> .status
                        |> .pendingKeys
                        |> Expect.equal "5"
            , test "on AZERTY the key dialog adds that digits need Shift" <|
                \_ ->
                    let
                        dialogsAfterKey layout =
                            Playthrough.start (Hall.init (Random.initialSeed 1))
                                |> Playthrough.sendAll (Hall.update { context | layout = layout })
                                    (List.map Hall.KeyPressed (List.repeat 12 "Enter" ++ [ "1", "5", "k", "8", "l", "k" ])
                                        ++ List.repeat 400 (Hall.Frame 16)
                                    )
                                |> countDialogLines layout
                    in
                    ( dialogsAfterKey SwissQwertz, dialogsAfterKey FrenchAzerty )
                        |> Expect.equal ( 2, 3 )
            ]
        , test "the easy hall's fastest route earns its one tart" <|
            \_ ->
                inEasyHall
                    -- k takes the key from the glass table (#86).
                    |> press "5k8lk"
                    |> wait
                    |> readStory
                    |> press "9l5j"
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "walking into the key on the table takes it; Alice stays in front of the table (#86)" <|
            \_ ->
                inEasyHall
                    |> press "5k8lk"
                    |> wait
                    |> .model
                    |> (\model -> ( model.hasKey, model.alice ))
                    |> Expect.equal ( True, { x = 9, y = 5 } )
        , test "a count stops at the table when it takes the key" <|
            \_ ->
                inEasyHall
                    |> press "5k8l"
                    |> wait
                    |> press "9k"
                    |> wait
                    |> .model
                    |> (\model -> ( model.hasKey, model.alice ))
                    |> Expect.equal ( True, { x = 9, y = 5 } )
        , test "the bottle is only to look at: walking into it does nothing" <|
            \_ ->
                inEasyHall
                    |> press "5k9lk"
                    |> wait
                    |> .model
                    |> (\model -> ( model.hasKey, model.alice ))
                    |> Expect.equal ( False, { x = 10, y = 5 } )
        , test "walking without counts still finishes, with one tart" <|
            \_ ->
                inEasyHall
                    |> press "kkkkkllllllllk"
                    |> wait
                    |> readStory
                    |> press "llllllllljjjjj"
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "keys pressed during the story do not move Alice" <|
            \_ ->
                start
                    |> press "kkk"
                    |> wait
                    |> .model
                    |> .alice
                    |> Expect.equal { x = 1, y = 10 }
        , test "walking into a wall stops the rest of a count" <|
            \_ ->
                inEasyHall
                    |> press "9h"
                    |> wait
                    |> .model
                    |> .alice
                    |> Expect.equal { x = 1, y = 10 }
        , test "the curtain stays locked without the key" <|
            \_ ->
                inEasyHall
                    |> press "17l"
                    |> wait
                    |> (\run -> ( Playthrough.finishedWith run, Playthrough.toastCount run ))
                    |> Expect.equal ( Nothing, 1 )
        , test "arrow keys show a hint instead of moving" <|
            \_ ->
                inEasyHall
                    |> send (Hall.KeyPressed "ArrowRight")
                    |> wait
                    |> (\run -> ( run.model.alice, Playthrough.toastCount run ))
                    |> Expect.equal ( { x = 1, y = 10 }, 1 )
        , test "a count cannot start with 0" <|
            \_ ->
                inEasyHall
                    |> press "0"
                    |> .model
                    |> .pendingCount
                    |> Expect.equal ""
        ]
