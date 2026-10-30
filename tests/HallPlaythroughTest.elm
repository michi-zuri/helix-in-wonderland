module HallPlaythroughTest exposing (suite)

{-| Play chapter 2 the way a child would: read the story, press keys, wait
for Alice to walk. No browser involved, just messages in, model and effects
out.
-}

import Chapter.Contract exposing (Context)
import Chapter.Hall as Hall
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


suite : Test
suite =
    describe "Chapter 2: The Hall of Doors"
        [ test "the easy hall's fastest route earns its one tart" <|
            \_ ->
                inEasyHall
                    |> press "5k8l"
                    |> wait
                    |> readStory
                    |> press "9l5j"
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "walking without counts still finishes, with one tart" <|
            \_ ->
                inEasyHall
                    |> press "kkkkkllllllll"
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
