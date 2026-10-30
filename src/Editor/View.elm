module Editor.View exposing (Decorations, modeName, position, view, viewWith)

{-| Draws a buffer the way Helix does: line numbers in the gutter, the
selection in purple, the cursor as a block (or, in Insert mode, as a bar in
front of the character where typing goes).

With soft wrap (`Core.withWrap`), a long line continues on more rows, marked
with ↪ in the gutter. `viewWith` can also draw `gw`'s jump labels and marks
a chapter puts on the text (chapter 4's currants).

-}

import Editor.Core as Core exposing (Buffer, Mode(..))
import Html exposing (Html)
import Html.Attributes as A


{-| What Helix's status line shows on the left: NOR or INS.
-}
modeName : Buffer -> String
modeName buffer =
    case Core.mode buffer of
        Normal ->
            "NOR"

        Insert ->
            "INS"


{-| What Helix's status line shows on the right: line:column.
-}
position : Buffer -> String
position buffer =
    let
        ( line, column ) =
            Core.cursorLineColumn buffer
    in
    String.fromInt line ++ ":" ++ String.fromInt column


type Look
    = Plain
    | Selected
    | Cursor
    | InsertionPoint
    | Marked String


{-| Extra things drawn over the text: `gw`'s labels (two letters over the
start of a word) and a chapter's marks (one character, with a CSS class).
The cursor stays visible on top of a mark.
-}
type alias Decorations =
    { labels : List { position : Int, label : String }
    , marks : List { position : Int, text : Char, class : String }
    }


view : Buffer -> Html msg
view =
    viewWith { labels = [], marks = [] }


viewWith : Decorations -> Buffer -> Html msg
viewWith decorations buffer =
    let
        ( cursorLine, _ ) =
            Core.cursorLineColumn buffer

        lookAt =
            lookFor buffer

        overlay =
            overlayFor decorations

        rowsOf line =
            case Core.wrapWidth buffer of
                Just width ->
                    chunks width line

                Nothing ->
                    [ line ]
    in
    Html.div [ A.class "editor" ]
        (Core.toString buffer
            |> linesWithOffsets
            |> List.indexedMap
                (\index ( offset, line ) ->
                    rowsOf (List.indexedMap (\column char -> ( offset + column, char )) (String.toList line))
                        |> List.indexedMap
                            (\row rowCells ->
                                viewLine (index + 1 == cursorLine)
                                    (if row == 0 then
                                        String.fromInt (index + 1)

                                     else
                                        "↪"
                                    )
                                    (cells lookAt overlay rowCells)
                            )
                )
            |> List.concat
        )


{-| A line's characters split into screen rows of `width`; the final space
(where the newline is) stays on the last row unless that row is full.
-}
chunks : Int -> List a -> List (List a)
chunks width items =
    if List.length items <= width then
        [ items ]

    else
        List.take width items :: chunks width (List.drop width items)


{-| What to draw at a position instead of the character, if anything.
-}
overlayFor : Decorations -> Int -> Maybe ( String, Char )
overlayFor decorations =
    let
        isMarked at =
            List.any (\mark -> mark.position == at) decorations.marks

        -- A label over a mark gets the mark's look too, so the player can
        -- tell which label belongs to it.
        fromLabels =
            decorations.labels
                |> List.concatMap
                    (\jump ->
                        let
                            class =
                                if isMarked jump.position then
                                    "jump-label on-mark"

                                else
                                    "jump-label"
                        in
                        List.indexedMap (\index char -> ( jump.position + index, ( class, char ) )) (String.toList jump.label)
                    )

        fromMarks =
            List.map (\mark -> ( mark.position, ( mark.class, mark.text ) )) decorations.marks

        all =
            fromLabels ++ fromMarks
    in
    \at ->
        all
            |> List.filter (\( markedAt, _ ) -> markedAt == at)
            |> List.head
            |> Maybe.map Tuple.second


{-| Each line with the position of its first character. The newline that
ends a line is kept as a final space, because the cursor can rest on it.
-}
linesWithOffsets : String -> List ( Int, String )
linesWithOffsets text =
    let
        lines =
            String.lines text
                -- The text always ends with a newline, which makes one empty line too many.
                |> List.reverse
                |> List.drop 1
                |> List.reverse

        withOffset line ( offset, done ) =
            ( offset + String.length line + 1, ( offset, line ++ " " ) :: done )
    in
    List.foldl withOffset ( 0, [] ) lines
        |> Tuple.second
        |> List.reverse


lookFor : Buffer -> Int -> Look
lookFor buffer =
    let
        ( first, last ) =
            Core.selectionRange buffer

        head =
            Core.head buffer
    in
    case Core.mode buffer of
        Insert ->
            \index ->
                if index == head then
                    InsertionPoint

                else
                    Plain

        Normal ->
            \index ->
                if index == head then
                    Cursor

                else if index >= first && index <= last then
                    Selected

                else
                    Plain


{-| Neighbouring characters that look the same are grouped into one run,
so a line becomes a handful of spans instead of one per character.
-}
cells : (Int -> Look) -> (Int -> Maybe ( String, Char )) -> List ( Int, Char ) -> List ( Look, String )
cells lookAt overlay positioned =
    positioned
        |> List.map
            (\( at, char ) ->
                case ( lookAt at, overlay at ) of
                    ( Cursor, _ ) ->
                        ( Cursor, char )

                    ( InsertionPoint, _ ) ->
                        ( InsertionPoint, char )

                    ( _, Just ( class, shown ) ) ->
                        ( Marked class, shown )

                    ( look, Nothing ) ->
                        ( look, char )
            )
        |> List.foldr
            (\( look, char ) runs ->
                case runs of
                    ( runLook, text ) :: rest ->
                        if runLook == look then
                            ( look, String.cons char text ) :: rest

                        else
                            ( look, String.fromChar char ) :: runs

                    [] ->
                        [ ( look, String.fromChar char ) ]
            )
            []


viewLine : Bool -> String -> List ( Look, String ) -> Html msg
viewLine isCurrent gutter runs =
    Html.div [ A.classList [ ( "editor-line", True ), ( "current", isCurrent ) ] ]
        [ Html.span [ A.class "editor-gutter" ] [ Html.text gutter ]
        , Html.span [ A.class "editor-code" ] (List.map viewRun runs)
        ]


viewRun : ( Look, String ) -> Html msg
viewRun ( look, text ) =
    case look of
        Plain ->
            Html.text text

        Selected ->
            Html.span [ A.class "selected" ] [ Html.text text ]

        Cursor ->
            Html.span [ A.class "cursor" ] [ Html.text text ]

        InsertionPoint ->
            Html.span [ A.class "insertion-point" ] [ Html.text text ]

        Marked class ->
            Html.span [ A.class class ] [ Html.text text ]
