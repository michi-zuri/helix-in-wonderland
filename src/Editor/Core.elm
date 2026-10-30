module Editor.Core exposing
    ( Buffer, Mode(..), Command(..)
    , fromString, apply, applyTimes
    , toString, mode, head, selectionRange, selectedText, wordUnderCursor, cursorLineColumn
    )

{-| A small, pure model of the Helix editor: text, one selection, two modes.

Helix works "selection first": a motion like `w` _selects_ something, and an
action like `d` then acts on whatever is selected. This module mirrors that.
Every function here is pure, so the whole editor can be tested without a
browser (see `tests/EditorCoreTest.elm`).


## Positions

A position is an index into the text. The text always ends with a newline,
and the cursor may rest on a line's newline character (Helix allows this too).
A selection runs from `anchor` to `head`, both ends included. When nothing is
"really" selected, anchor and head are equal: a one-character selection.

In Insert mode, `head` is the insertion point: typed text goes in front of
the character at `head`.

@docs Buffer, Mode, Command
@docs fromString, apply, applyTimes
@docs toString, mode, head, selectionRange, selectedText, wordUnderCursor, cursorLineColumn

-}

import Array exposing (Array)



-- TYPES


{-| The editor state. Opaque, so the invariants below can't be broken from
outside:

  - the text ends with `'\n'`
  - `0 <= anchor < length` and `0 <= head < length`
  - in Insert mode, `anchor == head`

-}
type Buffer
    = Buffer
        { chars : Array Char
        , anchor : Int
        , head : Int
        , mode : Mode
        , goalColumn : Int
        , history : List Snapshot
        }


type alias Snapshot =
    { chars : Array Char
    , anchor : Int
    , head : Int
    }


type Mode
    = Normal
    | Insert


{-| Everything the editor can do. The mapping from keys to commands lives in
`Editor.Keymap`, so chapters can decide which commands are unlocked.
-}
type Command
    = MoveLeft
    | MoveRight
    | MoveDown
    | MoveUp
    | SelectNextWordStart
    | SelectNextWordEnd
    | SelectPreviousWordStart
    | SelectLine
    | CollapseSelection
    | Delete
    | Change
    | Undo
    | InsertBefore
    | InsertAfter
    | ExitInsert
    | TypeChar Char
    | Backspace


type CharClass
    = Word
    | Punctuation
    | Whitespace
    | Newline


historyLimit : Int
historyLimit =
    100



-- CREATE AND READ


{-| Start with the cursor on the first character, in Normal mode. A missing
final newline is added.
-}
fromString : String -> Buffer
fromString text =
    Buffer
        { chars = Array.fromList (String.toList (withFinalNewline text))
        , anchor = 0
        , head = 0
        , mode = Normal
        , goalColumn = 0
        , history = []
        }


toString : Buffer -> String
toString (Buffer b) =
    String.fromList (Array.toList b.chars)


mode : Buffer -> Mode
mode (Buffer b) =
    b.mode


{-| The cursor position (in Insert mode: the insertion point).
-}
head : Buffer -> Int
head (Buffer b) =
    b.head


{-| The selected range as `( first, last )`, both included.
-}
selectionRange : Buffer -> ( Int, Int )
selectionRange (Buffer b) =
    ( min b.anchor b.head, max b.anchor b.head )


selectedText : Buffer -> String
selectedText ((Buffer b) as buffer) =
    let
        ( first, last ) =
            selectionRange buffer
    in
    String.fromList (Array.toList (Array.slice first (last + 1) b.chars))


{-| The whole word the cursor sits in, or `""` when it is not on a word.
-}
wordUnderCursor : Buffer -> String
wordUnderCursor (Buffer b) =
    if isClass b.chars Word b.head then
        let
            first =
                scanBackward (isClass b.chars Word) b.head + 1

            afterLast =
                scanForward (isClass b.chars Word) b.head
        in
        String.fromList (Array.toList (Array.slice first afterLast b.chars))

    else
        ""


{-| 1-based line and column of the cursor, as shown in Helix's status line.
-}
cursorLineColumn : Buffer -> ( Int, Int )
cursorLineColumn (Buffer b) =
    let
        newlinesBefore =
            Array.slice 0 b.head b.chars
                |> Array.filter (\c -> c == '\n')
                |> Array.length
    in
    ( newlinesBefore + 1, b.head - lineStart b.chars b.head + 1 )



-- APPLY A COMMAND


{-| A command with a count, as in `3x` or `5w`. Like Helix, moving,
selecting and undoing repeat; deleting, changing and inserting happen once
whatever the count. Counts above `largestCount` are treated as
`largestCount`.
-}
applyTimes : Int -> Command -> Buffer -> Buffer
applyTimes count command buffer =
    if isRepeatable command then
        List.foldl (\_ current -> apply command current) buffer (List.range 1 (clamp 1 largestCount count))

    else
        apply command buffer


largestCount : Int
largestCount =
    100


isRepeatable : Command -> Bool
isRepeatable command =
    List.member command
        [ MoveLeft, MoveRight, MoveDown, MoveUp, SelectNextWordStart, SelectNextWordEnd, SelectPreviousWordStart, SelectLine, Undo ]


{-| Run one command. Commands that make no sense in the current mode (typing
in Normal mode, `w` in Insert mode) leave the buffer unchanged.
-}
apply : Command -> Buffer -> Buffer
apply command ((Buffer b) as buffer) =
    case ( b.mode, command ) of
        ( Normal, MoveLeft ) ->
            moveTo (max (lineStart b.chars b.head) (b.head - 1)) buffer
                |> rememberColumn

        ( Normal, MoveRight ) ->
            moveTo
                (if charAt b.chars b.head == Just '\n' then
                    b.head

                 else
                    b.head + 1
                )
                buffer
                |> rememberColumn

        ( Normal, MoveDown ) ->
            moveTo (verticalTarget 1 b.chars b.goalColumn b.head) buffer

        ( Normal, MoveUp ) ->
            moveTo (verticalTarget -1 b.chars b.goalColumn b.head) buffer

        ( Normal, SelectNextWordStart ) ->
            select (nextWordStart b.chars b.head) buffer

        ( Normal, SelectNextWordEnd ) ->
            select (nextWordEnd b.chars b.head) buffer

        ( Normal, SelectPreviousWordStart ) ->
            select (previousWordStart b.chars b.head) buffer

        ( Normal, SelectLine ) ->
            select (Just (lineSelection b.chars (selectionRange buffer))) buffer

        ( Normal, CollapseSelection ) ->
            Buffer { b | anchor = b.head }

        ( Normal, Delete ) ->
            deleteSelection (saveHistory buffer)

        ( Normal, Change ) ->
            deleteSelection (saveHistory buffer)
                |> enterInsertAt (Tuple.first (selectionRange buffer))

        ( Normal, Undo ) ->
            undo buffer

        ( Normal, InsertBefore ) ->
            saveHistory buffer
                |> enterInsertAt (Tuple.first (selectionRange buffer))

        ( Normal, InsertAfter ) ->
            let
                last =
                    Tuple.second (selectionRange buffer)

                insertionPoint =
                    if charAt b.chars last == Just '\n' then
                        last

                    else
                        last + 1
            in
            saveHistory buffer
                |> enterInsertAt insertionPoint

        ( Insert, TypeChar char ) ->
            Buffer
                { b
                    | chars = insertAt b.head char b.chars
                    , head = b.head + 1
                    , anchor = b.head + 1
                }

        ( Insert, Backspace ) ->
            if b.head > 0 then
                Buffer
                    { b
                        | chars = removeRange (b.head - 1) (b.head - 1) b.chars |> ensureFinalNewline
                        , head = b.head - 1
                        , anchor = b.head - 1
                    }

            else
                buffer

        ( Insert, ExitInsert ) ->
            -- Like Helix, the cursor steps back onto the last typed character.
            let
                position =
                    max (lineStart b.chars b.head) (b.head - 1)
            in
            Buffer { b | mode = Normal, head = position, anchor = position }
                |> rememberColumn

        _ ->
            buffer



-- COMMAND HELPERS


moveTo : Int -> Buffer -> Buffer
moveTo position (Buffer b) =
    Buffer { b | head = position, anchor = position }


select : Maybe { anchor : Int, head : Int } -> Buffer -> Buffer
select maybeSelection ((Buffer b) as buffer) =
    case maybeSelection of
        Just s ->
            Buffer { b | anchor = s.anchor, head = s.head }
                |> rememberColumn

        Nothing ->
            buffer


rememberColumn : Buffer -> Buffer
rememberColumn (Buffer b) =
    Buffer { b | goalColumn = b.head - lineStart b.chars b.head }


{-| Where `j` (+1) or `k` (-1) lands: the same column on the next or previous
line, or that line's end when it is shorter.
-}
verticalTarget : Int -> Array Char -> Int -> Int -> Int
verticalTarget direction chars goalColumn position =
    let
        targetLineStart =
            if direction > 0 then
                let
                    nextStart =
                        lineEnd chars position + 1
                in
                if nextStart < Array.length chars then
                    Just nextStart

                else
                    Nothing

            else
                let
                    currentStart =
                        lineStart chars position
                in
                if currentStart > 0 then
                    Just (lineStart chars (currentStart - 1))

                else
                    Nothing
    in
    case targetLineStart of
        Just start ->
            min (start + goalColumn) (lineEnd chars start)

        Nothing ->
            position


saveHistory : Buffer -> Buffer
saveHistory (Buffer b) =
    Buffer
        { b
            | history =
                { chars = b.chars, anchor = b.anchor, head = b.head }
                    :: List.take (historyLimit - 1) b.history
        }


undo : Buffer -> Buffer
undo ((Buffer b) as buffer) =
    case b.history of
        snapshot :: older ->
            Buffer
                { b
                    | chars = snapshot.chars
                    , anchor = snapshot.anchor
                    , head = snapshot.head
                    , history = older
                }
                |> rememberColumn

        [] ->
            buffer


deleteSelection : Buffer -> Buffer
deleteSelection ((Buffer b) as buffer) =
    let
        ( first, last ) =
            selectionRange buffer

        remaining =
            removeRange first last b.chars |> ensureFinalNewline

        position =
            min first (Array.length remaining - 1)
    in
    Buffer { b | chars = remaining, anchor = position, head = position }
        |> rememberColumn


enterInsertAt : Int -> Buffer -> Buffer
enterInsertAt position (Buffer b) =
    let
        clamped =
            clamp 0 (Array.length b.chars - 1) position
    in
    Buffer { b | mode = Insert, head = clamped, anchor = clamped }



-- WORD MOTIONS
--
-- These follow Helix's behaviour for the common cases a beginner meets:
--   w  selects from the start of the next word up to (not including) the word
--      after it, so `w` then `d` removes a word and its trailing space.
--   e  selects to the end of the next word.
--   b  selects back to the start of the previous word.
-- If the cursor sits on the boundary between two character classes (the last
-- letter of a word, say), the motion starts one character further along, as
-- Helix does.


nextWordStart : Array Char -> Int -> Maybe { anchor : Int, head : Int }
nextWordStart chars position =
    let
        firstStep =
            if isClass chars Newline position || isBoundaryAhead chars position then
                position + 1

            else
                position

        -- Never start a selection on a newline: hop to the next line instead.
        start =
            stepIf (isClass chars Newline) 1 firstStep
    in
    classAt chars start
        |> Maybe.map
            (\startClass ->
                let
                    afterWord =
                        if startClass == Whitespace then
                            start

                        else
                            scanForward (isClass chars startClass) start

                    afterSpaces =
                        scanForward (isClass chars Whitespace) afterWord
                in
                { anchor = start, head = max start (afterSpaces - 1) }
            )


nextWordEnd : Array Char -> Int -> Maybe { anchor : Int, head : Int }
nextWordEnd chars position =
    let
        start =
            stepIf (isBoundaryAhead chars) 1 position

        wordStart =
            scanForward (\p -> isClass chars Whitespace p || isClass chars Newline p) start

        crossedLine =
            List.any (isClass chars Newline) (List.range start (wordStart - 1))
    in
    classAt chars wordStart
        |> Maybe.map
            (\wordClass ->
                { anchor =
                    if crossedLine then
                        wordStart

                    else
                        start
                , head = scanForward (isClass chars wordClass) wordStart - 1
                }
            )


previousWordStart : Array Char -> Int -> Maybe { anchor : Int, head : Int }
previousWordStart chars position =
    let
        start =
            stepIf (isBoundaryBehind chars) -1 position

        wordEnd =
            scanBackward (\p -> isClass chars Whitespace p || isClass chars Newline p) start

        crossedLine =
            List.any (isClass chars Newline) (List.range (wordEnd + 1) start)
    in
    classAt chars wordEnd
        |> Maybe.map
            (\wordClass ->
                { anchor =
                    if crossedLine then
                        wordEnd

                    else
                        start
                , head = scanBackward (isClass chars wordClass) wordEnd + 1
                }
            )


{-| `x` selects the whole line, newline included. Pressing it again while
whole lines are selected extends the selection by one more line.
-}
lineSelection : Array Char -> ( Int, Int ) -> { anchor : Int, head : Int }
lineSelection chars ( first, last ) =
    let
        start =
            lineStart chars first

        end =
            lineEnd chars last

        alreadyWholeLines =
            first == start && last == end
    in
    if alreadyWholeLines && end + 1 < Array.length chars then
        { anchor = start, head = lineEnd chars (end + 1) }

    else
        { anchor = start, head = end }



-- CHARACTERS AND LINES


charAt : Array Char -> Int -> Maybe Char
charAt chars position =
    Array.get position chars


classAt : Array Char -> Int -> Maybe CharClass
classAt chars position =
    Maybe.map classify (charAt chars position)


{-| Letters, digits and underscore form words. Every non-ASCII character
counts as a letter too, so words like "Tränen" or "Lücke" stay whole.
-}
classify : Char -> CharClass
classify char =
    if char == '\n' then
        Newline

    else if char == ' ' || char == '\t' then
        Whitespace

    else if Char.isAlphaNum char || char == '_' || Char.toCode char > 127 then
        Word

    else
        Punctuation


isClass : Array Char -> CharClass -> Int -> Bool
isClass chars class position =
    classAt chars position == Just class


isBoundaryAhead : Array Char -> Int -> Bool
isBoundaryAhead chars position =
    case ( classAt chars position, classAt chars (position + 1) ) of
        ( Just here, Just next ) ->
            here /= next

        _ ->
            False


isBoundaryBehind : Array Char -> Int -> Bool
isBoundaryBehind chars position =
    case ( classAt chars position, classAt chars (position - 1) ) of
        ( Just here, Just previous ) ->
            here /= previous

        _ ->
            False


stepIf : (Int -> Bool) -> Int -> Int -> Int
stepIf condition step position =
    if condition position then
        position + step

    else
        position


{-| The first position at or after `position` where `condition` is false.
Stops at the end of the text because `condition` is false outside it.
-}
scanForward : (Int -> Bool) -> Int -> Int
scanForward condition position =
    if condition position then
        scanForward condition (position + 1)

    else
        position


scanBackward : (Int -> Bool) -> Int -> Int
scanBackward condition position =
    if condition position then
        scanBackward condition (position - 1)

    else
        position


lineStart : Array Char -> Int -> Int
lineStart chars position =
    scanBackward (\p -> p > 0 && charAt chars (p - 1) /= Just '\n') position


{-| Position of the newline that ends the line containing `position`.
-}
lineEnd : Array Char -> Int -> Int
lineEnd chars position =
    scanForward (\p -> p < Array.length chars - 1 && charAt chars p /= Just '\n') position



-- ARRAY EDITING


insertAt : Int -> Char -> Array Char -> Array Char
insertAt position char chars =
    Array.append
        (Array.push char (Array.slice 0 position chars))
        (Array.slice position (Array.length chars) chars)


removeRange : Int -> Int -> Array Char -> Array Char
removeRange first last chars =
    Array.append
        (Array.slice 0 first chars)
        (Array.slice (last + 1) (Array.length chars) chars)


ensureFinalNewline : Array Char -> Array Char
ensureFinalNewline chars =
    if Array.get (Array.length chars - 1) chars == Just '\n' then
        chars

    else
        Array.push '\n' chars


withFinalNewline : String -> String
withFinalNewline text =
    if String.endsWith "\n" text then
        text

    else
        text ++ "\n"
