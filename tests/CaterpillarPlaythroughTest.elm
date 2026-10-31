module CaterpillarPlaythroughTest exposing (suite)

{-| Play chapter 7 in all three difficulties: fill gaps, change wrong words,
add line endings and delete a line that doesn't belong.
-}

import Chapter.Caterpillar as Caterpillar
import Chapter.Contract exposing (Context)
import Editor.Core as Core exposing (Mode(..))
import Editor.Session as Session
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Texts exposing (Texts)


contextIn : Language -> Context
contextIn language =
    { texts = Texts.forLanguage language, layout = SwissQwertz, escapeKeys = Nothing }


context : Context
context =
    contextIn English


pressIn : Context -> List String -> Run Caterpillar.Model -> Run Caterpillar.Model
pressIn ctx pressed =
    Playthrough.sendAll (Caterpillar.update ctx) (List.map Caterpillar.KeyPressed pressed)


press : List String -> Run Caterpillar.Model -> Run Caterpillar.Model
press =
    pressIn context


readStory : Run Caterpillar.Model -> Run Caterpillar.Model
readStory =
    press (List.repeat 12 "Enter")


{-| Read the story, then pick a difficulty: "1" easy, "2" challenge, "3" mad.
-}
startIn : Context -> String -> Run Caterpillar.Model
startIn ctx difficulty =
    Playthrough.start Caterpillar.init
        |> pressIn ctx (List.repeat 12 "Enter")
        |> pressIn ctx [ difficulty ]


start : String -> Run Caterpillar.Model
start =
    startIn context


{-| A route written as one string per step: every character is a key, `⎋` is
Esc and `⌫` is Backspace. "3w" is "3" then "w"; "cold⎋" changes the selection
to "old" and goes back to Normal mode.
-}
keys : List String -> List String
keys steps =
    String.concat steps
        |> String.toList
        |> List.map
            (\char ->
                case char of
                    '⎋' ->
                        "Escape"

                    '⌫' ->
                        "Backspace"

                    _ ->
                        String.fromChar char
            )


typeIn : String -> List String
typeIn word =
    List.map String.fromChar (String.toList word)


toLineStart : List String
toLineStart =
    List.repeat 40 "h"


{-| Easy: walk to each gap, `i`, type the word. `leave` decides how Insert
mode is left (normally Esc).
-}
fillTheGaps : List String -> List String
fillTheGaps leave =
    List.concat
        [ List.repeat 9 "l" ++ [ "i" ] ++ typeIn "you" ++ [ "Escape" ]
        , "j" :: toLineStart ++ [ "l", "l", "l", "i" ] ++ typeIn "hardly" ++ [ "Escape" ]
        , "j" :: toLineStart ++ List.repeat 11 "l" ++ [ "i" ] ++ typeIn "temper" ++ leave
        ]


{-| Challenge: a wrong word (`w`, `c`), a gap (`i`), a missing line ending
(`gl`, `a`) and one more wrong word.
-}
challengeRoute : Language -> List String
challengeRoute language =
    case language of
        English ->
            [ "6w", "cthat⎋", "j", "gh", "9l", "iyourself⎋", "j", "gla Alice⎋", "j", "gh", "3w", "ctemper⎋" ]

        German ->
            [ "4w", "cdamit⎋", "j", "gh", "9l", "idich⎋", "j", "gla Alice⎋", "j", "gh", "3w", "cFassung⎋" ]


{-| Mad: our own best way through Alice's garbled poem, with the keys the
book has taught (no `e` or `b`). Its keystrokes are the "Our best" number.
-}
madRoute : Language -> List String
madRoute language =
    case language of
        English ->
            [ "3w", "cold⎋", "6w", "cyoung ⎋", "j", "6h", "ivery ⎋", "j", "6h", "w", "cstand ⎋", "3w", "chead ⎋", "jjxd", "gla son,⎋", "j", "gh", "5w", "cinjure ⎋", "j", "7h", "iperfectly ⎋", "j", "i again.\"⎋" ]

        German ->
            [ "3w", "calt⎋", "7w", "cjunge ⎋", "j", "15h", "ischon ⎋", "j", "16h", "w", "cstehst ⎋", "3w", "cKopf⎋", "jjxd", "gla Sohn,⎋", "j", "4h", "w", "cHirn⎋", "j", "11h", "i sicher⎋", "j", "w", "a Stirn.\"⎋" ]


nameOf : Language -> String
nameOf language =
    case language of
        English ->
            "English"

        German ->
            "German"


keystrokesOf : Run Caterpillar.Model -> Int
keystrokesOf run =
    run.model.work |> Maybe.map (.session >> Session.keystrokes) |> Maybe.withDefault 0


textOf : Run Caterpillar.Model -> String
textOf run =
    run.model.work |> Maybe.map (.session >> Session.buffer >> Core.toString) |> Maybe.withDefault ""


modeOf : Run Caterpillar.Model -> Maybe Mode
modeOf run =
    run.model.work |> Maybe.map (.session >> Session.buffer >> Core.mode)


panelHas : String -> Run Caterpillar.Model -> Expect.Expectation
panelHas wanted run =
    (Caterpillar.view context run.model).panel |> Query.fromHtml |> Query.has [ text wanted ]


panelHasNot : String -> Run Caterpillar.Model -> Expect.Expectation
panelHasNot unwanted run =
    (Caterpillar.view context run.model).panel |> Query.fromHtml |> Query.hasNot [ text unwanted ]


hints : Texts -> List String
hints texts =
    [ texts.caterpillar.hintFind, texts.caterpillar.hintChange, texts.caterpillar.hintAppend, texts.caterpillar.hintTyping ]


{-| Exactly one of the hints is in the panel, and it is `wanted`.
-}
onlyHint : String -> Run Caterpillar.Model -> Expect.Expectation
onlyHint wanted =
    Expect.all
        (List.map
            (\hint ->
                if hint == wanted then
                    panelHas hint

                else
                    panelHasNot hint
            )
            (hints context.texts)
        )


suite : Test
suite =
    describe "Chapter 7: Advice from a Caterpillar"
        [ describe "the difficulty sets the most tarts there are"
            [ test "easy: filling every gap and pressing Esc earns one tart" <|
                \_ ->
                    start "1"
                        |> press (fillTheGaps [ "Escape" ])
                        |> readStory
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
            , describe "challenge: two tarts"
                (List.map
                    (\language ->
                        test (nameOf language) <|
                            \_ ->
                                startIn (contextIn language) "2"
                                    |> pressIn (contextIn language) (keys (challengeRoute language))
                                    |> readStory
                                    |> Playthrough.finishedWith
                                    |> Expect.equal (Just 2)
                    )
                    [ English, German ]
                )
            , describe "mad: our best way earns three tarts in the keystrokes shown"
                (List.map
                    (\language ->
                        let
                            ctx =
                                contextIn language
                        in
                        test (nameOf language) <|
                            \_ ->
                                startIn ctx "3"
                                    |> pressIn ctx (keys (madRoute language))
                                    |> (\run -> ( keystrokesOf run, readStory run |> Playthrough.finishedWith ))
                                    |> Expect.equal ( ctx.texts.caterpillar.madBest, Just 3 )
                    )
                    [ English, German ]
                )
            ]
        , test "mad: the line that doesn't belong is gone, the poem has eight lines" <|
            \_ ->
                start "3"
                    |> press (keys (madRoute English))
                    |> textOf
                    |> String.lines
                    |> List.filter (not << String.isEmpty)
                    |> List.length
                    |> Expect.equal 8
        , test "the player's own escape keys work like Esc: ;; on mad earns three tarts too" <|
            \_ ->
                let
                    withEscapeKeys =
                        { context | escapeKeys = Just ( ';', ';' ) }
                in
                startIn withEscapeKeys "3"
                    |> pressIn withEscapeKeys
                        (List.concatMap
                            (\key ->
                                if key == "Escape" then
                                    [ ";", ";" ]

                                else
                                    [ key ]
                            )
                            (keys (madRoute English))
                        )
                    |> pressIn withEscapeKeys (List.repeat 12 "Enter")
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "too slow between the two escape keys, and they are just typed" <|
            \_ ->
                let
                    withEscapeKeys =
                        { context | escapeKeys = Just ( ';', ';' ) }

                    send =
                        Playthrough.send (Caterpillar.update withEscapeKeys)
                in
                startIn withEscapeKeys "1"
                    |> send (Caterpillar.KeyPressed "i")
                    |> send (Caterpillar.KeyPressed ";")
                    |> send (Caterpillar.Frame 400)
                    |> send (Caterpillar.KeyPressed ";")
                    |> modeOf
                    |> Expect.equal (Just Insert)
        , test "the chapter waits until the player is back in Normal mode" <|
            \_ ->
                start "1"
                    |> press (fillTheGaps [])
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , test "mad: a few typos fixed with Backspace still earn three tarts" <|
            \_ ->
                start "3"
                    |> press (keys [ "3w", "coldd⌫⎋" ])
                    |> press (keys (List.drop 2 (madRoute English)))
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "mad: lots of Backspace costs tarts" <|
            \_ ->
                start "3"
                    |> press (keys [ "3w", "cxxxxxxxxxx⌫⌫⌫⌫⌫⌫⌫⌫⌫⌫old⎋" ])
                    |> press (keys (List.drop 2 (madRoute English)))
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "the Esc fallback tip appears only the first time" <|
            \_ ->
                start "1"
                    |> press (fillTheGaps [ "Escape" ])
                    |> Playthrough.toastCount
                    |> Expect.equal 1
        , test "e and b wait for a later chapter" <|
            \_ ->
                start "3"
                    |> press [ "l", "l", "e", "b" ]
                    |> .model
                    |> .work
                    |> Maybe.map (.session >> Session.buffer >> Core.selectedText)
                    |> Expect.equal (Just "o")
        , describe "challenge and mad: one hint at a time, the one that helps"
            [ test "first: compare and find what's wrong" <|
                \_ -> start "3" |> onlyHint context.texts.caterpillar.hintFind
            , test "with a word selected: c changes it" <|
                \_ -> start "3" |> press (keys [ "3w" ]) |> onlyHint context.texts.caterpillar.hintChange
            , test "in Insert mode: type, then Esc" <|
                \_ -> start "3" |> press (keys [ "3w", "c" ]) |> onlyHint context.texts.caterpillar.hintTyping
            , test "at the end of a line: a appends" <|
                \_ -> start "2" |> press (keys [ "jj", "gl" ]) |> onlyHint context.texts.caterpillar.hintAppend
            , test "easy keeps its checklist, without these hints" <|
                \_ -> start "1" |> press (keys [ "3w" ]) |> panelHasNot context.texts.caterpillar.hintChange
            ]
        , test "the text follows the language until a difficulty is picked" <|
            \_ ->
                Playthrough.start Caterpillar.init
                    |> readStory
                    |> pressIn (contextIn German) [ "3" ]
                    |> press [ "l" ]
                    |> textOf
                    |> Expect.equal ((contextIn German).texts.caterpillar.madStart ++ "\n")
        ]
