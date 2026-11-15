module Pixel.Canvas exposing (Side(..), grow, trim, trimFrames)

{-| Making a picture bigger in the pixel editor, and trimming it when it is
sent (#134).

The grid can grow by an empty row or column on any side. When the picture
is sent, every empty row and column at its edges is trimmed away, so the
proposal shows exactly what was drawn: a picture can grow and shrink.
Pictures that take turns in the game (Alice's frames) share one size; when
a proposal is merged, it is padded back to that size or its place in the
game is adjusted.

-}

import Array exposing (Array)


type Side
    = Top
    | Bottom
    | Left
    | Right


empty : Char
empty =
    '.'


{-| One empty row or column more on `side`.
-}
grow : Side -> Array (Array Char) -> Array (Array Char)
grow side pixels =
    let
        width =
            Array.get 0 pixels |> Maybe.map Array.length |> Maybe.withDefault 0

        emptyRow =
            Array.repeat width empty
    in
    case side of
        Top ->
            Array.append (Array.fromList [ emptyRow ]) pixels

        Bottom ->
            Array.push emptyRow pixels

        Left ->
            Array.map (Array.append (Array.fromList [ empty ])) pixels

        Right ->
            Array.map (Array.push empty) pixels


{-| The rows of the picture without the empty rows and columns at its
edges. A picture with nothing drawn stays as it is.
-}
trim : List String -> List String
trim rows =
    let
        isEmpty line =
            String.all ((==) empty) line

        keptRows =
            rows
                |> dropWhile isEmpty
                |> List.reverse
                |> dropWhile isEmpty
                |> List.reverse

        columns =
            List.map isEmpty (transpose keptRows)

        fromLeft =
            List.length (takeWhileTrue columns)

        fromRight =
            List.length (takeWhileTrue (List.reverse columns))
    in
    if List.isEmpty keptRows then
        rows

    else
        List.map (String.dropLeft fromLeft >> String.dropRight fromRight) keptRows


{-| Trim several frames of one picture together: an edge is cut only where
every frame is empty there, so the frames stay the same size and keep
their places (#136).
-}
trimFrames : List (List String) -> List (List String)
trimFrames frames =
    let
        -- One picture with paint wherever any frame has paint.
        together =
            List.foldl
                (\frame combined ->
                    List.map2 (\a b -> String.fromList (List.map2 overlay (String.toList a) (String.toList b))) frame combined
                )
                (List.head frames |> Maybe.withDefault [])
                frames

        overlay a b =
            if a /= empty then
                a

            else
                b

        kept =
            trim together

        rowsCut =
            margins together kept
    in
    if kept == together then
        frames

    else
        List.map (cut rowsCut) frames


{-| How much `trim` cut from each side, found by where the kept rows sit
in the whole picture.
-}
margins : List String -> List String -> { top : Int, left : Int, width : Int, height : Int }
margins whole kept =
    let
        isEmpty line =
            String.all ((==) empty) line

        top =
            List.length (takeWhileTrue (List.map isEmpty whole))

        columns =
            List.map isEmpty (transpose whole)

        left =
            List.length (takeWhileTrue columns)
    in
    { top = top
    , left = left
    , width = List.head kept |> Maybe.map String.length |> Maybe.withDefault 0
    , height = List.length kept
    }


cut : { top : Int, left : Int, width : Int, height : Int } -> List String -> List String
cut area frame =
    frame
        |> List.drop area.top
        |> List.take area.height
        |> List.map (String.slice area.left (area.left + area.width))


takeWhileTrue : List Bool -> List Bool
takeWhileTrue bools =
    case bools of
        True :: rest ->
            True :: takeWhileTrue rest

        _ ->
            []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile isDropped items =
    case items of
        first :: rest ->
            if isDropped first then
                dropWhile isDropped rest

            else
                items

        [] ->
            []


transpose : List String -> List String
transpose rows =
    let
        width =
            List.map String.length rows |> List.maximum |> Maybe.withDefault 0
    in
    List.range 0 (width - 1)
        |> List.map (\column -> String.fromList (List.filterMap (String.slice column (column + 1) >> String.uncons >> Maybe.map Tuple.first) rows))
