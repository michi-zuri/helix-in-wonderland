module EditorRepeatTest exposing (suite)

{-| `.` repeats the last change, as in Helix (#74): a `c`, `i` or `a` with
everything typed until Insert mode ends, or a `d`.
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


textAndMode : Session -> ( String, Mode )
textAndMode session =
    ( Core.toString (Session.buffer session), Core.mode (Session.buffer session) )


stars : Session
stars =
    Session.start "\n***\n***\n***"


suite : Test
suite =
    describe ". repeats the last change"
        [ test "e c, a word, j k, then e . changes the next stars into the same word" <|
            \_ ->
                press [ "e", "c", "s", "h", "e", "j", "k", "e", "." ] stars
                    |> textAndMode
                    |> Expect.equal ( "\nshe\nshe\n***\n", Normal )
        , test "it works after Esc too, and again and again" <|
            \_ ->
                press [ "e", "c", "a", "Escape", "e", ".", "e", "." ] stars
                    |> textAndMode
                    |> Expect.equal ( "\na\na\na\n", Normal )
        , test "Backspace is part of the change" <|
            \_ ->
                press [ "e", "c", "a", "x", "Backspace", "s", "j", "k", "e", "." ] stars
                    |> textAndMode
                    |> Expect.equal ( "\nas\nas\n***\n", Normal )
        , test "a j typed with j j or j and another key is part of it" <|
            \_ ->
                Session.start "\n*\n*"
                    |> press [ "e", "c", "j", "j", "j", "a", "j", "k", "e", "." ]
                    |> textAndMode
                    |> Expect.equal ( "\njja\njja\n", Normal )
        , test "i repeats as an insert in front of the cursor" <|
            \_ ->
                Session.start "ab\ncd"
                    |> press [ "i", "x", "j", "k", "j", "." ]
                    |> Session.buffer
                    |> Core.toString
                    |> Expect.equal "xab\ncxd\n"
        , test "d repeats a delete" <|
            \_ ->
                Session.start "one two three"
                    |> press [ "w", "d", "w", "." ]
                    |> Session.buffer
                    |> Core.toString
                    |> Expect.equal "three\n"
        , test "a change undone from outside (withBuffer) is forgotten; the one before it stays" <|
            \_ ->
                let
                    afterFirst =
                        press [ "e", "c", "a", "j", "k", "e" ] stars

                    half =
                        press [ "c", "b" ] afterFirst
                in
                Session.withBuffer (Session.buffer afterFirst) half
                    |> press [ "." ]
                    |> textAndMode
                    |> Expect.equal ( "\na\na\n***\n", Normal )
        , test "nothing changed yet: . does nothing" <|
            \_ -> press [ "." ] stars |> textAndMode |> Expect.equal ( "\n***\n***\n***\n", Normal )
        , test ". is one keystroke" <|
            \_ -> press [ "e", "c", "a", "j", "k", "e", "." ] stars |> Session.keystrokes |> Expect.equal 7
        ]
