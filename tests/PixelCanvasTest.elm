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


grown : List Side -> List String
grown sides =
    List.foldl Canvas.grow (picture |> List.map (String.toList >> Array.fromList) |> Array.fromList) sides
        |> Array.toList
        |> List.map (Array.toList >> String.fromList)


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
                    |> Expect.equal [ "......", "..ab..", "..cd..", "......" ]
        , test "every empty row and column at the edges is trimmed, the picture's own too: pictures can shrink" <|
            \_ ->
                grown [ Top, Top, Bottom, Left, Right, Right ]
                    |> Canvas.trim
                    |> Expect.equal [ "ab", "cd" ]
        , test "an erased edge is trimmed too" <|
            \_ ->
                picture
                    |> paint 1 0 '.'
                    |> paint 1 1 '.'
                    |> Canvas.trim
                    |> Expect.equal [ "b", "d" ]
        , test "empty rows and columns inside the picture stay" <|
            \_ ->
                grown [ Left, Left, Right ]
                    |> paint 0 1 'x'
                    |> Canvas.trim
                    |> Expect.equal [ "...ab", "x..cd" ]
        , test "an added row with paint on it stays" <|
            \_ ->
                grown [ Top, Bottom, Bottom ]
                    |> paint 2 4 'x'
                    |> Canvas.trim
                    |> Expect.equal [ "ab", "cd", "..", ".x" ]
        , test "a picture with nothing drawn stays as it is" <|
            \_ -> Canvas.trim [ "..", ".." ] |> Expect.equal [ "..", ".." ]
        ]
