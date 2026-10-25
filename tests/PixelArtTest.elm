module PixelArtTest exposing (suite)

{-| Checks every picture in the game, so a proposal with a typo in it is
caught before it reaches the screen.
-}

import Expect
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "pixel art"
        (List.concatMap
            (\{ name, sprite } ->
                let
                    rows =
                        Sprite.rows sprite

                    letters =
                        List.map Tuple.first (Sprite.palette sprite)
                in
                [ test (name ++ ": every row is equally wide") <|
                    \_ ->
                        rows
                            |> List.map String.length
                            |> List.filter (\width -> width /= (Sprite.size sprite).width)
                            |> Expect.equal []
                , test (name ++ ": every letter in the picture has a colour") <|
                    \_ ->
                        rows
                            |> String.concat
                            |> String.toList
                            |> List.filter (\char -> char /= '.' && not (List.member char letters))
                            |> Expect.equal []
                ]
            )
            Sprites.catalogue
        )
