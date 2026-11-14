module EditorCoreTest exposing (suite)

{-| Tests read like Helix sessions: start with some text, press keys, look at
what is selected or what the text became.
-}

import Editor.Core as Core exposing (Buffer, Mode(..))
import Editor.Keymap as Keymap
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, test)


{-| Press keys one after another. Each list item is one key, as the browser
names it: `"w"`, `"Escape"`, `"Backspace"`.
-}
press : List String -> Buffer -> Buffer
press keys buffer =
    List.foldl
        (\key current ->
            case Keymap.commandFor (Core.mode current) key of
                Just command ->
                    Core.apply command current

                Nothing ->
                    current
        )
        buffer
        keys


{-| `typeIn "hello"` = press h, e, l, l, o.
-}
typeIn : String -> List String
typeIn text =
    List.map String.fromChar (String.toList text)


poem : String
poem =
    "How doth the little splash crocodile\nImprove his shining tail,\n"


{-| Cursor on the "s" of "splash" (index 20).
-}
onSplash : Buffer
onSplash =
    Core.fromString poem |> press (List.repeat 20 "l")


suite : Test
suite =
    describe "Editor.Core"
        [ describe "moving"
            [ test "l moves right, h moves left" <|
                \_ ->
                    Core.fromString "abc" |> press [ "l", "l", "h" ] |> Core.head |> Expect.equal 1
            , test "l on the last character goes onto the line break, then to the next line, as in Helix (#119)" <|
                \_ ->
                    Core.fromString "abc\ndef"
                        |> press [ "l", "l" ]
                        |> (\onC -> [ onC, press [ "l" ] onC, press [ "l", "l" ] onC ])
                        |> List.map Core.cursorLineColumn
                        |> Expect.equal [ ( 1, 3 ), ( 1, 4 ), ( 2, 1 ) ]
            , test "h at the start of a line goes back onto the line break above, then its last character" <|
                \_ ->
                    Core.fromString "abc\ndef"
                        |> press [ "j" ]
                        |> (\onD -> [ onD, press [ "h" ] onD, press [ "h", "h" ] onD ])
                        |> List.map Core.cursorLineColumn
                        |> Expect.equal [ ( 2, 1 ), ( 1, 4 ), ( 1, 3 ) ]
            , test "five l from a go to e: across the line break" <|
                \_ -> Core.fromString "abc\ndef" |> press (List.repeat 5 "l") |> Core.cursorLineColumn |> Expect.equal ( 2, 2 )
            , test "h stops at the very start of the text, l at its very end" <|
                \_ ->
                    ( Core.fromString "ab\ncd" |> press [ "h" ] |> Core.cursorLineColumn
                    , Core.fromString "ab\ncd" |> press (List.repeat 9 "l") |> Core.cursorLineColumn
                    )
                        |> Expect.equal ( ( 1, 1 ), ( 2, 3 ) )
            , test "j keeps the column when the next line is long enough" <|
                \_ ->
                    Core.fromString "abcd\nefgh" |> press [ "l", "l", "j" ] |> Core.cursorLineColumn |> Expect.equal ( 2, 3 )
            , test "j passes a short line and remembers the column" <|
                \_ ->
                    Core.fromString "abcd\ne\nfghi"
                        |> press [ "l", "l", "l", "j", "j" ]
                        |> Core.cursorLineColumn
                        |> Expect.equal ( 3, 4 )
            ]
        , describe "selecting words (selection first)"
            [ test "w selects the word under the cursor and its trailing space" <|
                \_ ->
                    onSplash |> press [ "w" ] |> Core.selectedText |> Expect.equal "splash "
            , test "w from the space before a word selects that word" <|
                \_ ->
                    onSplash |> press [ "h", "w" ] |> Core.selectedText |> Expect.equal "splash "
            , test "pressing w again selects the next word, not more" <|
                \_ ->
                    onSplash |> press [ "w", "w" ] |> Core.selectedText |> Expect.equal "crocodile"
            , test "e selects to the end of the word, without the space" <|
                \_ ->
                    onSplash |> press [ "e" ] |> Core.selectedText |> Expect.equal "splash"
            , test "b selects back to the start of the previous word" <|
                \_ ->
                    onSplash |> press [ "b" ] |> Core.selectedText |> Expect.equal "little "
            , test "w at the end of a line continues on the next line" <|
                \_ ->
                    Core.fromString "ab\ncd ef" |> press [ "l", "w" ] |> Core.selectedText |> Expect.equal "cd "
            , test "punctuation is its own word" <|
                \_ ->
                    Core.fromString "tail, and" |> press [ "w", "w" ] |> Core.selectedText |> Expect.equal ", "
            , test "umlauts belong to words" <|
                \_ ->
                    Core.fromString "die Tränen fliessen" |> press [ "w", "w" ] |> Core.selectedText |> Expect.equal "Tränen "
            ]
        , describe "deleting"
            [ test "w then d removes a word cleanly" <|
                \_ ->
                    onSplash |> press [ "w", "d" ] |> Core.toString |> Expect.equal "How doth the little crocodile\nImprove his shining tail,\n"
            , test "x selects the whole line, x again adds the next line" <|
                \_ ->
                    Core.fromString "one\ntwo\nthree" |> press [ "x", "x" ] |> Core.selectedText |> Expect.equal "one\ntwo\n"
            , test "x then d removes the line" <|
                \_ ->
                    Core.fromString "keep\ntears tears\nkeep too" |> press [ "j", "x", "d" ] |> Core.toString |> Expect.equal "keep\nkeep too\n"
            , test "u brings back what d removed" <|
                \_ ->
                    onSplash |> press [ "w", "d", "u" ] |> Core.toString |> Expect.equal poem
            , test "deleting everything still leaves a final newline" <|
                \_ ->
                    Core.fromString "only line" |> press [ "x", "d" ] |> Core.toString |> Expect.equal "\n"
            ]
        , describe "inserting"
            [ test "i inserts before the cursor" <|
                \_ ->
                    Core.fromString "Who are ?"
                        |> press (List.repeat 8 "l" ++ [ "i" ] ++ typeIn "you" ++ [ "Escape" ])
                        |> Core.toString
                        |> Expect.equal "Who are you?\n"
            , test "a inserts after the cursor" <|
                \_ ->
                    Core.fromString "ab" |> press ("a" :: typeIn "X") |> Core.toString |> Expect.equal "aXb\n"
            , test "i switches to Insert mode, Escape back to Normal" <|
                \_ ->
                    Core.fromString "ab"
                        |> press [ "i" ]
                        |> Core.mode
                        |> Expect.equal Insert
            , test "after Escape the cursor stays where it was, after the typed text, as in Helix (#115)" <|
                \_ ->
                    Core.fromString "ab" |> press ("i" :: typeIn "xy" ++ [ "Escape" ]) |> Core.head |> Expect.equal 2
            , test "i, Escape, i: the cursor is still in the same place" <|
                \_ ->
                    Core.fromString "abcd"
                        |> press [ "l", "l", "i" ]
                        |> (\inserting -> ( Core.head inserting, inserting |> press [ "Escape", "i" ] |> Core.head ))
                        |> Expect.equal ( 2, 2 )
            , test "typing, Escape, i: typing goes on where it left off" <|
                \_ ->
                    Core.fromString "ad" |> press ("l" :: "i" :: typeIn "b" ++ [ "Escape", "i" ] ++ typeIn "c") |> Core.toString |> Expect.equal "abcd\n"
            , test "c, Escape, i: the cursor is still after the new text" <|
                \_ ->
                    Core.fromString "a?d" |> press ("l" :: "c" :: typeIn "bc" ++ [ "Escape", "i" ] ++ typeIn "x") |> Core.toString |> Expect.equal "abcxd\n"
            , test "Backspace removes the character before the cursor" <|
                \_ ->
                    Core.fromString "ab" |> press ("a" :: typeIn "xyz" ++ [ "Backspace", "Escape" ]) |> Core.toString |> Expect.equal "axyb\n"
            , test "c replaces the selection" <|
                \_ ->
                    onSplash |> press ([ "e", "c" ] ++ typeIn "tiny" ++ [ "Escape" ]) |> Core.toString |> Expect.equal "How doth the little tiny crocodile\nImprove his shining tail,\n"
            , test "one u undoes a whole insert session" <|
                \_ ->
                    Core.fromString "ab" |> press ("i" :: typeIn "xyz" ++ [ "Escape", "u" ]) |> Core.toString |> Expect.equal "ab\n"
            , test "typing in Normal mode does not change the text" <|
                \_ ->
                    Core.fromString "ab" |> press (typeIn "zzz") |> Core.toString |> Expect.equal "ab\n"
            ]
        , describe "invariants, for any text and any keys"
            [ fuzz2 textFuzzer keysFuzzer "the text always ends with a newline" <|
                \text keys ->
                    Core.fromString text |> press keys |> Core.toString |> String.endsWith "\n" |> Expect.equal True
            , fuzz2 textFuzzer keysFuzzer "the selection always stays inside the text" <|
                \text keys ->
                    let
                        buffer =
                            Core.fromString text |> press keys

                        ( first, last ) =
                            Core.selectionRange buffer
                    in
                    Expect.all
                        [ \_ -> Expect.atLeast 0 first
                        , \_ -> Expect.atMost (String.length (Core.toString buffer) - 1) last
                        , \_ -> Expect.atMost last first
                        ]
                        ()
            , fuzz2 textFuzzer keysFuzzer "in Insert mode the selection is a single point" <|
                \text keys ->
                    let
                        buffer =
                            Core.fromString text |> press keys

                        ( first, last ) =
                            Core.selectionRange buffer
                    in
                    if Core.mode buffer == Insert then
                        Expect.equal first last

                    else
                        Expect.pass
            , fuzz2 textFuzzer keysFuzzer "d followed by u restores the text" <|
                \text keys ->
                    let
                        before =
                            Core.fromString text |> press (keys ++ [ "Escape" ])
                    in
                    before
                        |> press [ "d", "u" ]
                        |> Core.toString
                        |> Expect.equal (Core.toString before)
            ]
        ]


textFuzzer : Fuzzer String
textFuzzer =
    Fuzz.list (Fuzz.oneOfValues [ 'a', 'b', 'ä', ' ', ',', '\n' ])
        |> Fuzz.map String.fromList


keysFuzzer : Fuzzer (List String)
keysFuzzer =
    Fuzz.list
        (Fuzz.oneOfValues
            [ "h", "j", "k", "l", "w", "e", "b", "x", "d", "c", "u", "i", "a", ";", "Escape", "Backspace", "Enter", "z", " " ]
        )
