module Pixel.Canvas exposing (Added, Side(..), grow, none, trim)

{-| Making a picture bigger in the pixel editor, and trimming it again
(#134).

The grid can grow by an empty row or column on any side. When the picture
is sent, the rows and columns that were added and stayed empty are trimmed
away. The picture's own rows and columns are never trimmed, even empty
ones: some pictures have empty edges on purpose (Alice's first and last
columns), and the game places pictures by their size.

-}

import Array exposing (Array)


type Side
    = Top
    | Bottom
    | Left
    | Right


{-| How many rows and columns were added on each side.
-}
type alias Added =
    { top : Int
    , bottom : Int
    , left : Int
    , right : Int
    }


none : Added
none =
    { top = 0, bottom = 0, left = 0, right = 0 }


empty : Char
empty =
    '.'


{-| One empty row or column more on `side`.
-}
grow : Side -> { pixels : Array (Array Char), added : Added } -> { pixels : Array (Array Char), added : Added }
grow side { pixels, added } =
    let
        width =
            Array.get 0 pixels |> Maybe.map Array.length |> Maybe.withDefault 0

        emptyRow =
            Array.repeat width empty
    in
    case side of
        Top ->
            { pixels = Array.append (Array.fromList [ emptyRow ]) pixels, added = { added | top = added.top + 1 } }

        Bottom ->
            { pixels = Array.push emptyRow pixels, added = { added | bottom = added.bottom + 1 } }

        Left ->
            { pixels = Array.map (Array.append (Array.fromList [ empty ])) pixels, added = { added | left = added.left + 1 } }

        Right ->
            { pixels = Array.map (Array.push empty) pixels, added = { added | right = added.right + 1 } }


{-| The rows of the picture without the added rows and columns that stayed
empty. An added row or column with paint on it stays, and so does
everything between it and the picture.
-}
trim : Added -> List String -> List String
trim added rows =
    let
        isEmpty line =
            String.all ((==) empty) line

        columns =
            transpose rows

        dropEmpty most lines =
            dropWhileAtMost most isEmpty lines

        keptRows =
            rows
                |> dropEmpty added.top
                |> List.reverse
                |> dropEmpty added.bottom
                |> List.reverse

        fromLeft =
            List.length (List.take added.left (takeWhileTrue (List.map isEmpty columns)))

        fromRight =
            List.length (List.take added.right (takeWhileTrue (List.map isEmpty (List.reverse columns))))
    in
    List.map (String.dropLeft fromLeft >> String.dropRight fromRight) keptRows


takeWhileTrue : List Bool -> List Bool
takeWhileTrue bools =
    case bools of
        True :: rest ->
            True :: takeWhileTrue rest

        _ ->
            []


dropWhileAtMost : Int -> (a -> Bool) -> List a -> List a
dropWhileAtMost most isDropped items =
    case items of
        first :: rest ->
            if most > 0 && isDropped first then
                dropWhileAtMost (most - 1) isDropped rest

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
