module CaterpillarPlaythroughTest exposing (suite)

{-| Play chapter 6: fill the gaps in Insert mode.
-}

import Chapter.Caterpillar as Caterpillar
import Chapter.Contract exposing (Context)
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Test exposing (Test, describe, test)
import Texts


context : Context
context =
    { texts = Texts.forLanguage English, layout = SwissQwertz, escapeKeys = Nothing }


press : List String -> Run Caterpillar.Model -> Run Caterpillar.Model
press keys =
    Playthrough.sendAll (Caterpillar.update context) (List.map Caterpillar.KeyPressed keys)


readStory : Run Caterpillar.Model -> Run Caterpillar.Model
readStory =
    press (List.repeat 12 "Enter")


typeIn : String -> List String
typeIn word =
    List.map String.fromChar (String.toList word)


toLineStart : List String
toLineStart =
    List.repeat 40 "h"


{-| Each gap: walk to it, `i`, type the word. `leave` decides how Insert mode
is left (normally Esc).
-}
fillTheGaps : List String -> List String
fillTheGaps leave =
    List.concat
        [ List.repeat 9 "l" ++ [ "i" ] ++ typeIn "you" ++ [ "Escape" ]
        , "j" :: toLineStart ++ [ "l", "l", "l", "i" ] ++ typeIn "hardly" ++ [ "Escape" ]
        , "j" :: toLineStart ++ List.repeat 11 "l" ++ [ "i" ] ++ typeIn "temper" ++ leave
        ]


suite : Test
suite =
    describe "Chapter 6: Advice from a Caterpillar"
        [ test "filling every gap and pressing Esc earns three tarts" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> press (fillTheGaps [ "Escape" ])
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "the player's own escape keys work like Esc: ;; earns three tarts too" <|
            \_ ->
                let
                    withEscapeKeys =
                        { context | escapeKeys = Just ( ';', ';' ) }
                in
                Playthrough.start Caterpillar.init
                    |> Playthrough.sendAll (Caterpillar.update withEscapeKeys) (List.repeat 12 (Caterpillar.KeyPressed "Enter"))
                    |> Playthrough.sendAll (Caterpillar.update withEscapeKeys)
                        (List.map Caterpillar.KeyPressed
                            (List.concatMap
                                (\key ->
                                    if key == "Escape" then
                                        [ ";", ";" ]

                                    else
                                        [ key ]
                                )
                                (fillTheGaps [ "Escape" ])
                            )
                        )
                    |> Playthrough.sendAll (Caterpillar.update withEscapeKeys) (List.repeat 12 (Caterpillar.KeyPressed "Enter"))
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "the chapter waits until the player is back in Normal mode" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> press (fillTheGaps [])
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , test "a few typos fixed with Backspace still earn three tarts" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> press (List.repeat 9 "l" ++ [ "i" ] ++ typeIn "yoo" ++ [ "Backspace" ] ++ typeIn "u" ++ [ "Escape" ])
                    |> press (List.drop 13 (fillTheGaps [ "Escape" ]))
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "lots of Backspace costs tarts" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> press (List.repeat 9 "l" ++ [ "i" ] ++ typeIn "xxxxxxxxxx" ++ List.repeat 10 "Backspace" ++ typeIn "you" ++ [ "Escape" ])
                    |> press (List.drop 13 (fillTheGaps [ "Escape" ]))
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "the Esc fallback tip appears only the first time" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> press (fillTheGaps [ "Escape" ])
                    |> Playthrough.toastCount
                    |> Expect.equal 1
        ]
