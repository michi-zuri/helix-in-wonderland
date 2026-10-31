module Chapter.Curiouser.Table exposing
    ( text, wrapWidth
    , Spot, positionOf, spotOf
    , Trick(..), place, placeAway, isInSight
    )

{-| The table chapter 5 plays on: lines of cake crumbs (`.`) of different
lengths, two so long that they wrap onto more screen rows. Every crumb is a
word of its own, so `gw` gives each one a label; two spaces between crumbs
keep the two-letter labels apart. Crumbs sit on every third column (the
wrap width too), so `gj` and `gk` always land on a crumb.

`place` decides where a currant appears so that one of Helix's `g` jumps is
the quick way there: at the end of the cursor's line (`gl`, `gh`), on the
start of the first or last line (`gg`, `ge`), or on the real line after a long
wrapped one (`gj`, `gk`). Cheeky currants (`g.`) start far away and out of
sight, and run back to where the last one was squashed once the cursor gets
onto their line or column (`isInSight`).

@docs text, wrapWidth
@docs Spot, positionOf, spotOf
@docs Trick, place, placeAway, isInSight

-}

import Random


{-| The table is the same in every language: crumbs only.
-}
text : String
text =
    String.join "\n" rows ++ "\n"


{-| Soft wrap: lines longer than this continue on the next screen row.
-}
wrapWidth : Int
wrapWidth =
    30


rows : List String
rows =
    [ crumbs 10
    , crumbs 8
    , crumbs 25
    , crumbs 9
    , crumbs 18
    , crumbs 7
    , crumbs 10
    ]


crumbs : Int -> String
crumbs count =
    String.join "  " (List.repeat count ".")



-- SPOTS


{-| A place on the table: line and column, both counted from 0.
-}
type alias Spot =
    { line : Int, column : Int }


positionOf : Spot -> Int
positionOf spot =
    (rows |> List.take spot.line |> List.map (\row -> String.length row + 1) |> List.sum) + spot.column


spotOf : Int -> Spot
spotOf position =
    let
        find line remaining rest =
            case rest of
                row :: more ->
                    if remaining <= String.length row || List.isEmpty more then
                        { line = line, column = remaining }

                    else
                        find (line + 1) (remaining - String.length row - 1) more

                [] ->
                    { line = line, column = remaining }
    in
    find 0 position rows


rowText : Int -> String
rowText line =
    rows |> List.drop line |> List.head |> Maybe.withDefault ""


{-| The columns of a line's crumbs.
-}
crumbColumns : Int -> List Int
crumbColumns line =
    String.toList (rowText line)
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, char ) -> char /= ' ')
        |> List.map Tuple.first


allSpots : List Spot
allSpots =
    List.range 0 (List.length rows - 1)
        |> List.concatMap (\line -> List.map (\column -> { line = line, column = column }) (crumbColumns line))


isWrapped : Int -> Bool
isWrapped line =
    String.length (rowText line) > wrapWidth


{-| Where a spot is on screen: its screen row (counting wrapped rows) and
its column within that row.
-}
onScreen : Spot -> ( Int, Int )
onScreen spot =
    let
        rowsBefore =
            rows
                |> List.take spot.line
                |> List.map (\row -> String.length row // wrapWidth + 1)
                |> List.sum
    in
    ( rowsBefore + spot.column // wrapWidth, modBy wrapWidth spot.column )


{-| A shy, cheeky currant runs off once the cursor is on its line (the
same real line, or the same row on screen) or in its column on screen.
-}
isInSight : Spot -> Spot -> Bool
isInSight a b =
    let
        ( rowA, columnA ) =
            onScreen a

        ( rowB, columnB ) =
            onScreen b
    in
    a.line == b.line || rowA == rowB || columnA == columnB


isFar : Spot -> Spot -> Bool
isFar a b =
    let
        ( rowA, columnA ) =
            onScreen a

        ( rowB, columnB ) =
            onScreen b
    in
    abs (rowA - rowB) >= 3 || abs (columnA - columnB) >= 14



-- PLACING CURRANTS


{-| Which of Helix's jumps a currant is placed for. `LastChange` is a
cheeky currant, caught with `g.`.
-}
type Trick
    = LineEnds
    | FileEnds
    | RealLines
    | LastChange


{-| Where a currant for `trick` appears, given where the cursor is.
-}
place : Trick -> Spot -> Random.Generator Spot
place trick cursor =
    case trick of
        LineEnds ->
            Random.constant (lineEnd cursor)

        FileEnds ->
            Random.constant (fileEnd cursor)

        RealLines ->
            Random.constant (realLine cursor)

        LastChange ->
            placeAway cursor


{-| The end of the cursor's line further from the cursor: its first or
last crumb.
-}
lineEnd : Spot -> Spot
lineEnd cursor =
    let
        columns =
            crumbColumns cursor.line

        first =
            List.head columns |> Maybe.withDefault 0

        last =
            List.reverse columns |> List.head |> Maybe.withDefault 0
    in
    if abs (last - cursor.column) >= abs (first - cursor.column) then
        { cursor | column = last }

    else
        { cursor | column = first }


{-| The start of the first or the last line, whichever is further from
the cursor: where `gg` and `ge` land.
-}
fileEnd : Spot -> Spot
fileEnd cursor =
    let
        lastLine =
            List.length rows - 1
    in
    if cursor.line * 2 < lastLine then
        { line = lastLine, column = 0 }

    else
        { line = 0, column = 0 }


{-| From a long wrapped line, the next real line (or the one before, from
the last line), at the crumb nearest the cursor's column. From a short
line, across the nearest wrapped line, so the way crosses its rows.
-}
realLine : Spot -> Spot
realLine cursor =
    let
        lastLine =
            List.length rows - 1

        target =
            if isWrapped cursor.line then
                if cursor.line < lastLine then
                    cursor.line + 1

                else
                    cursor.line - 1

            else
                let
                    wrapped =
                        List.range 0 lastLine
                            |> List.filter isWrapped
                            |> List.sortBy (\line -> abs (line - cursor.line))
                            |> List.head
                            |> Maybe.withDefault 0
                in
                if wrapped > cursor.line then
                    min lastLine (wrapped + 1)

                else
                    max 0 (wrapped - 1)

        column =
            crumbColumns target
                |> List.sortBy (\c -> abs (c - cursor.column))
                |> List.head
                |> Maybe.withDefault 0
    in
    { line = target, column = column }


{-| Somewhere far from the cursor and out of its sight: not on its line,
row or column.
-}
placeAway : Spot -> Random.Generator Spot
placeAway cursor =
    case List.filter (\spot -> isFar cursor spot && not (isInSight cursor spot)) allSpots of
        first :: others ->
            Random.uniform first others

        [] ->
            Random.constant cursor
