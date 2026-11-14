module EditorEscapeKeysTest exposing (suite)

{-| Leaving Insert mode with `j k`, as this Helix config sets up (#110):

    [keys.insert.j]
    k = "normal_mode"
    j = [":insert-output printf j", "move_char_right"]

No timeout: `j` waits for the next key, however long it takes.

-}

import Editor.Core as Core exposing (Mode(..))
import Editor.Session as Session exposing (Session)
import Expect
import Test exposing (Test, describe, test)


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


start : Session
start =
    Session.start "Who are ?\n"


textAndMode : Session -> ( String, Mode )
textAndMode session =
    ( Core.toString (Session.buffer session), Core.mode (Session.buffer session) )


suite : Test
suite =
    describe "j k leaves Insert mode"
        [ test "j k in Insert mode goes back to Normal mode and leaves no j behind" <|
            \_ -> press [ "i", "y", "o", "u", "j", "k" ] start |> textAndMode |> Expect.equal ( "youWho are ?\n", Normal )
        , test "j alone waits: nothing typed yet, still Insert mode, and the menu is open" <|
            \_ ->
                press [ "i", "j" ] start
                    |> (\session -> ( textAndMode session, Session.isAfterJ session ))
                    |> Expect.equal ( ( "Who are ?\n", Insert ), True )
        , test "j j types one j" <|
            \_ -> press [ "i", "j", "j" ] start |> textAndMode |> Expect.equal ( "jWho are ?\n", Insert )
        , test "j followed by another key types both" <|
            \_ -> press [ "i", "j", "a" ] start |> textAndMode |> Expect.equal ( "jaWho are ?\n", Insert )
        , test "after j, Esc types the j and leaves Insert mode" <|
            \_ -> press [ "i", "j", "Escape" ] start |> textAndMode |> Expect.equal ( "jWho are ?\n", Normal )
        , test "the menu closes after the next key" <|
            \_ -> press [ "i", "j", "a" ] start |> Session.isAfterJ |> Expect.equal False
        , test "in Normal mode j moves down as always" <|
            \_ ->
                Session.start "one\ntwo\n"
                    |> press [ "j", "k", "j" ]
                    |> Session.buffer
                    |> Core.cursorLineColumn
                    |> Expect.equal ( 2, 1 )
        , test "j k counts two keystrokes" <|
            \_ -> press [ "i", "j", "k" ] start |> Session.keystrokes |> Expect.equal 3
        ]
