module EditorEscapeKeysTest exposing (suite)

{-| The player's own escape keys (`Editor.Session.pressWith`): two keys
that leave Insert mode like Esc, in Insert mode only.
-}

import Editor.Core as Core exposing (Mode(..))
import Editor.Session as Session exposing (Session)
import Expect
import Test exposing (Test, describe, test)


{-| Press keys with `pause` milliseconds between them.
-}
pressPausing : Float -> List String -> Session -> Session
pressPausing pause keys session =
    List.foldl
        (\key current ->
            case Session.pressWith (Just { keys = ( ';', ';' ), sinceLastKey = pause }) (always True) key current of
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


{-| Quick typing: 100 ms between keys.
-}
press : List String -> Session -> Session
press =
    pressPausing 100


start : Session
start =
    Session.start "Who are ?\n"


textAndMode : Session -> ( String, Mode )
textAndMode session =
    ( Core.toString (Session.buffer session), Core.mode (Session.buffer session) )


suite : Test
suite =
    describe "Escape keys"
        [ test ";; in Insert mode goes back to Normal mode and leaves no ; behind" <|
            \_ -> press [ "i", "y", "o", "u", ";", ";" ] start |> textAndMode |> Expect.equal ( "youWho are ?\n", Normal )
        , test "one ; is typed as text" <|
            \_ -> press [ "i", ";" ] start |> textAndMode |> Expect.equal ( ";Who are ?\n", Insert )
        , test "; followed by another key types both" <|
            \_ -> press [ "i", ";", "x" ] start |> textAndMode |> Expect.equal ( ";xWho are ?\n", Insert )
        , test ";;; leaves Insert mode once; the third ; is a Normal-mode key" <|
            \_ -> press [ "i", ";", ";", ";" ] start |> textAndMode |> Expect.equal ( "Who are ?\n", Normal )
        , test "in Normal mode ;; does nothing to the text" <|
            \_ -> press [ "l", ";", ";" ] start |> textAndMode |> Expect.equal ( "Who are ?\n", Normal )
        , test "a pause longer than the timeout between the two keys types both" <|
            \_ -> pressPausing (Session.escapeTimeout + 1) [ "i", ";", ";" ] start |> textAndMode |> Expect.equal ( ";;Who are ?\n", Insert )
        , test "a pause of exactly the timeout still counts" <|
            \_ -> pressPausing Session.escapeTimeout [ "i", ";", ";" ] start |> textAndMode |> Expect.equal ( "Who are ?\n", Normal )
        , test "plain press has no escape keys" <|
            \_ ->
                List.foldl
                    (\key current ->
                        case Session.press (always True) key current of
                            Session.Ran ran ->
                                ran.session

                            _ ->
                                current
                    )
                    start
                    [ "i", ";", ";" ]
                    |> textAndMode
                    |> Expect.equal ( ";;Who are ?\n", Insert )
        ]
