module EditorCountTest exposing (suite)

{-| Numbered commands, as in Helix: digits before a command repeat it.
Played through `Editor.Session`, which collects the count.
-}

import Editor.Core as Core
import Editor.Session as Session exposing (Session)
import Expect
import Test exposing (Test, describe, test)


{-| Press keys one after another, everything unlocked.
-}
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


poem : Session
poem =
    Session.start "How doth the little splash crocodile\nImprove his shining tail,\nAnd pour the waters\nOn every golden scale!\n"


selected : Session -> String
selected session =
    Core.selectedText (Session.buffer session)


text : Session -> String
text session =
    Core.toString (Session.buffer session)


suite : Test
suite =
    describe "Numbered commands"
        [ test "5w selects the fifth word ahead, like pressing w five times" <|
            \_ ->
                ( selected (press [ "5", "w" ] poem), selected (press (List.repeat 5 "w") poem) )
                    |> Expect.equal ( "splash ", "splash " )
        , test "3x selects three lines" <|
            \_ ->
                press [ "3", "x" ] poem
                    |> selected
                    |> Expect.equal "How doth the little splash crocodile\nImprove his shining tail,\nAnd pour the waters\n"
        , test "2j moves down two lines" <|
            \_ ->
                press [ "2", "j" ] poem
                    |> Session.buffer
                    |> Core.cursorLineColumn
                    |> Expect.equal ( 3, 1 )
        , test "d deletes once, whatever the count" <|
            \_ ->
                press [ "x", "3", "d" ] poem
                    |> text
                    |> Expect.equal "Improve his shining tail,\nAnd pour the waters\nOn every golden scale!\n"
        , test "2u undoes two changes" <|
            \_ ->
                press [ "x", "d", "x", "d", "2", "u" ] poem
                    |> text
                    |> Expect.equal (text poem)
        , test "counts of two digits work: 12l" <|
            \_ ->
                press [ "1", "2", "l" ] poem
                    |> Session.buffer
                    |> Core.cursorLineColumn
                    |> Expect.equal ( 1, 13 )
        , test "a count can't start with 0" <|
            \_ ->
                press [ "0" ] poem
                    |> Session.pendingCount
                    |> Expect.equal ""
        , test "the count shows while it is typed, and is gone after the command" <|
            \_ ->
                ( Session.pendingCount (press [ "1", "2" ] poem), Session.pendingCount (press [ "1", "2", "l" ] poem) )
                    |> Expect.equal ( "12", "" )
        , test "Esc drops a count" <|
            \_ ->
                press [ "3", "Escape", "x" ] poem
                    |> selected
                    |> Expect.equal "How doth the little splash crocodile\n"
        , test "a key that is no command drops the count" <|
            \_ ->
                press [ "3", "z", "x" ] poem
                    |> selected
                    |> Expect.equal "How doth the little splash crocodile\n"
        , test "a locked command drops the count too" <|
            \_ ->
                let
                    withoutX =
                        \command -> command /= Core.SelectLine

                    step key session =
                        case Session.press withoutX key session of
                            Session.Ran ran ->
                                ran.session

                            Session.Waiting next ->
                                next

                            Session.Locked next ->
                                next

                            Session.Ignored next ->
                                next
                in
                poem
                    |> step "3"
                    |> step "x"
                    |> Session.pendingCount
                    |> Expect.equal ""
        , test "digits count as keystrokes" <|
            \_ ->
                press [ "3", "x", "d" ] poem
                    |> Session.keystrokes
                    |> Expect.equal 3
        , describe "gh: go to the start of the line"
            [ test "gh jumps to the start of the line, from anywhere on it" <|
                \_ ->
                    press [ "j", "1", "2", "l", "g", "h" ] poem
                        |> Session.buffer
                        |> Core.cursorLineColumn
                        |> Expect.equal ( 2, 1 )
            , test "it leaves nothing selected" <|
                \_ -> press [ "3", "w", "g", "h" ] poem |> selected |> Expect.equal "H"
            , test "the g waits for the next key" <|
                \_ -> press [ "g" ] poem |> Session.isWaiting |> Expect.equal True
            , test "another key after g cancels it, and does nothing" <|
                \_ ->
                    press [ "l", "g", "z", "x" ] poem
                        |> Expect.all
                            [ Session.isWaiting >> Expect.equal False
                            , selected >> Expect.equal "How doth the little splash crocodile\n"
                            ]
            , test "g and h are two keystrokes" <|
                \_ -> press [ "g", "h" ] poem |> Session.keystrokes |> Expect.equal 2
            ]
        , test "in Insert mode digits are typed, not counted" <|
            \_ ->
                press [ "i", "4", "2", "Escape" ] poem
                    |> text
                    |> String.startsWith "42How"
                    |> Expect.equal True
        ]
