module PixelCanvasTest exposing (suite)

{-| Growing a picture in the pixel editor, and trimming it when it is sent
(#134).
-}

import Array
import Expect
import Pixel.Canvas as Canvas exposing (Side(..))
import Test exposing (Test, describe, test)


picture : List String
picture =
    [ ".ab."
    , ".cd."
    ]


grown : List Side -> { pixels : Array.Array (Array.Array Char), added : Canvas.Added }
grown sides =
    List.foldl Canvas.grow { pixels = picture |> List.map (String.toList >> Array.fromList) |> Array.fromList, added = Canvas.none } sides


rows : { a | pixels : Array.Array (Array.Array Char) } -> List String
rows canvas =
    canvas.pixels |> Array.toList |> List.map (Array.toList >> String.fromList)


paint : Int -> Int -> Char -> List String -> List String
paint x y char =
    List.indexedMap
        (\row line ->
            if row == y then
                String.left x line ++ String.fromChar char ++ String.dropLeft (x + 1) line

            else
                line
        )


suite : Test
suite =
    describe "a bigger canvas in the pixel editor"
        [ test "a row on top, a row below, a column on each side: empty pixels" <|
            \_ ->
                grown [ Top, Bottom, Left, Right ]
                    |> rows
                    |> Expect.equal [ "......", "..ab..", "..cd..", "......" ]
        , test "added rows and columns that stayed empty are trimmed" <|
            \_ ->
                grown [ Top, Top, Bottom, Left, Right, Right ]
                    |> (\canvas -> Canvas.trim canvas.added (rows canvas))
                    |> Expect.equal picture
        , test "the picture's own empty edges stay" <|
            \_ ->
                Canvas.trim Canvas.none picture |> Expect.equal picture
        , test "an added column with paint on it stays, and so does the empty one between it and the picture" <|
            \_ ->
                grown [ Left, Left, Right ]
                    |> (\canvas -> Canvas.trim canvas.added (paint 0 1 'x' (rows canvas)))
                    |> Expect.equal [ "...ab.", "x..cd." ]
        , test "an added row with paint on it stays" <|
            \_ ->
                grown [ Top, Bottom, Bottom ]
                    |> (\canvas -> Canvas.trim canvas.added (paint 2 4 'x' (rows canvas)))
                    |> Expect.equal [ ".ab.", ".cd.", "....", "..x." ]
        ]
