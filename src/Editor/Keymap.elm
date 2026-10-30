module Editor.Keymap exposing (commandFor, gotoCommand)

{-| Translate a pressed key into an editor command, the way Helix does.

Keys arrive as the browser's `KeyboardEvent.key` string (`"w"`, `"Escape"`,
`"Backspace"`), which already reflects the player's keyboard layout. So `w`
means "the key that types w", whether that is QWERTY, QWERTZ or AZERTY.

@docs commandFor, gotoCommand

-}

import Editor.Core exposing (Command(..), Mode(..))


commandFor : Mode -> String -> Maybe Command
commandFor mode key =
    case mode of
        Normal ->
            normalCommand key

        Insert ->
            insertCommand key


normalCommand : String -> Maybe Command
normalCommand key =
    case key of
        "h" ->
            Just MoveLeft

        "j" ->
            Just MoveDown

        "k" ->
            Just MoveUp

        "l" ->
            Just MoveRight

        "w" ->
            Just SelectNextWordStart

        "e" ->
            Just SelectNextWordEnd

        "b" ->
            Just SelectPreviousWordStart

        "x" ->
            Just SelectLine

        ";" ->
            Just CollapseSelection

        "Escape" ->
            Just CollapseSelection

        "d" ->
            Just Delete

        "c" ->
            Just Change

        "u" ->
            Just Undo

        "i" ->
            Just InsertBefore

        "a" ->
            Just InsertAfter

        _ ->
            Nothing


{-| The key after `g` in Normal mode (Helix's goto mode): `gh` goes to the
start of the line.
-}
gotoCommand : String -> Maybe Command
gotoCommand key =
    case key of
        "h" ->
            Just GotoLineStart

        _ ->
            Nothing


insertCommand : String -> Maybe Command
insertCommand key =
    case key of
        "Escape" ->
            Just ExitInsert

        "Backspace" ->
            Just Backspace

        "Enter" ->
            Just (TypeChar '\n')

        _ ->
            case String.toList key of
                [ char ] ->
                    Just (TypeChar char)

                _ ->
                    Nothing
