module EditorGotoTest exposing (suite)

{-| Helix's goto mode (`g` then a key) and soft wrap, played through
`Editor.Session` like a player would.
-}

import Editor.Core as Core
import Editor.Session as Session exposing (Session)
import Editor.View
import Expect
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)


press : List String -> Session -> Session
press keys session =
    List.foldl
        (\key current ->
            case Session.press (always True) key current of
                Session.Ran ran ->
                    ran.session

                Session.Waiting next ->
                    next

                Session.Locked next ->
                    next

                Session.Ignored next ->
                    next
        )
        session
        keys


cursor : Session -> ( Int, Int )
cursor session =
    Core.cursorLineColumn (Session.buffer session)


selected : Session -> String
selected session =
    Core.selectedText (Session.buffer session)


lines : Session
lines =
    Session.start "the cake was marked\n    with currants\nEAT ME\n"


{-| One line of 25 characters and a short one, shown 10 characters wide:
the long line takes three rows.
-}
wrapped : Session
wrapped =
    Session.fromBuffer (Core.withWrap 10 (Core.fromString "abcdefghijklmnopqrstuvwxy\nshort\n"))


suite : Test
suite =
    describe "Goto mode and soft wrap"
        [ test "gl goes to the last character of the line" <|
            \_ -> press [ "g", "l" ] lines |> cursor |> Expect.equal ( 1, 19 )
        , test "gh goes back to the start" <|
            \_ -> press [ "g", "l", "g", "h" ] lines |> cursor |> Expect.equal ( 1, 1 )
        , test "gs goes to the first character that isn't a space" <|
            \_ -> press [ "j", "g", "h", "g", "s" ] lines |> cursor |> Expect.equal ( 2, 5 )
        , test "on a line without spaces in front, gs is like gh" <|
            \_ -> press [ "2", "j", "l", "l", "g", "s" ] lines |> cursor |> Expect.equal ( 3, 1 )
        , describe "gg and ge: first and last line"
            [ test "ge goes to the start of the last line" <|
                \_ -> press [ "l", "l", "g", "e" ] lines |> cursor |> Expect.equal ( 3, 1 )
            , test "gg goes back to the start of the first line" <|
                \_ -> press [ "g", "e", "l", "g", "g" ] lines |> cursor |> Expect.equal ( 1, 1 )
            , test "with a number, gg goes to that line: 2gg" <|
                \_ -> press [ "2", "g", "g" ] lines |> cursor |> Expect.equal ( 2, 1 )
            , test "a number past the end goes to the last line" <|
                \_ -> press [ "9", "g", "g" ] lines |> cursor |> Expect.equal ( 3, 1 )
            , test "other goto keys ignore a number" <|
                \_ -> press [ "2", "g", "l" ] lines |> cursor |> Expect.equal ( 1, 19 )
            ]
        , describe "g. goes back to the last change"
            [ test "after deleting, then walking away" <|
                \_ -> press [ "j", "x", "d", "k", "l", "l", "g", "." ] lines |> cursor |> Expect.equal ( 2, 1 )
            , test "after something outside changed a character (as a game does)" <|
                \_ ->
                    Session.fromBuffer (Core.replaceChar 25 '*' (Core.fromString "the cake was marked\n    with currants\n"))
                        |> press [ "g", "." ]
                        |> cursor
                        |> Expect.equal ( 2, 6 )
            , test "with no change yet, it stays put" <|
                \_ -> press [ "l", "g", "." ] lines |> cursor |> Expect.equal ( 1, 2 )
            ]
        , describe "soft wrap: j and k move by screen row, gj and gk by real line"
            [ test "j moves one row down inside the long line" <|
                \_ -> press [ "l", "l", "j" ] wrapped |> cursor |> Expect.equal ( 1, 13 )
            , test "j three times reaches the next line" <|
                \_ -> press [ "l", "l", "j", "j", "j" ] wrapped |> cursor |> Expect.equal ( 2, 3 )
            , test "gj jumps straight to the next line" <|
                \_ -> press [ "l", "l", "g", "j" ] wrapped |> cursor |> Expect.equal ( 2, 3 )
            , test "k from the next line lands on the last row of the long line" <|
                \_ -> press [ "g", "j", "l", "k" ] wrapped |> cursor |> Expect.equal ( 1, 22 )
            , test "gk jumps to the long line's same column" <|
                \_ -> press [ "g", "j", "l", "g", "k" ] wrapped |> cursor |> Expect.equal ( 1, 2 )
            , test "without soft wrap, j and gj are the same" <|
                \_ -> ( cursor (press [ "j" ] lines), cursor (press [ "g", "j" ] lines) ) |> Expect.equal ( ( 2, 1 ), ( 2, 1 ) )
            , test "the view draws the long line on three rows, marked ↪" <|
                \_ ->
                    Editor.View.view (Session.buffer wrapped)
                        |> Query.fromHtml
                        |> Query.findAll [ class "editor-gutter", text "↪" ]
                        |> Query.count (Expect.equal 2)
            ]
        , describe "gw: jump to a word by its label"
            [ test "gw labels every word, in order: aa, ab, ..." <|
                \_ ->
                    press [ "g", "w" ] lines
                        |> Session.jumpLabels
                        |> List.map .label
                        |> Expect.equal [ "aa", "ab", "ac", "ad", "ae", "af", "ag", "ah" ]
            , test "typing a label selects its word" <|
                \_ -> press [ "g", "w", "a", "f" ] lines |> selected |> Expect.equal "currants"
            , test "after the first letter, only the matching labels stay" <|
                \_ -> press [ "g", "w", "a" ] lines |> Session.jumpLabels |> List.length |> Expect.equal 8
            , test "a letter that matches no label cancels" <|
                \_ ->
                    press [ "g", "w", "z" ] lines
                        |> Expect.all
                            [ Session.jumpLabels >> Expect.equal []
                            , Session.isWaiting >> Expect.equal False
                            , cursor >> Expect.equal ( 1, 1 )
                            ]
            , test "Esc cancels" <|
                \_ -> press [ "g", "w", "Escape" ] lines |> Session.isWaiting |> Expect.equal False
            , test "the view draws the labels over the words" <|
                \_ ->
                    Editor.View.viewWith { labels = Session.jumpLabels (press [ "g", "w" ] lines), marks = [] } (Session.buffer lines)
                        |> Query.fromHtml
                        |> Query.findAll [ class "jump-label" ]
                        |> Query.count (Expect.atLeast 8)
            ]
        ]
