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
            (\{ name, frames } ->
                List.indexedMap
                    (\index sprite ->
                        let
                            label =
                                name ++ ", frame " ++ String.fromInt (index + 1)

                            rows =
                                Sprite.rows sprite

                            letters =
                                List.map Tuple.first (Sprite.palette sprite)
                        in
                        [ test (label ++ ": every row is equally wide") <|
                            \_ ->
                                rows
                                    |> List.map String.length
                                    |> List.filter (\width -> width /= (Sprite.size sprite).width)
                                    |> Expect.equal []
                        , test (label ++ ": every letter in the picture has a colour") <|
                            \_ ->
                                rows
                                    |> String.concat
                                    |> String.toList
                                    |> List.filter (\char -> char /= '.' && not (List.member char letters))
                                    |> Expect.equal []
                        ]
                    )
                    frames
                    |> List.concat
                    |> (::)
                        (test (name ++ ": all frames have the same size, so they take turns in place (#136)") <|
                            \_ ->
                                frames
                                    |> List.map Sprite.size
                                    |> List.filter (\size -> Just size /= Maybe.map Sprite.size (List.head frames))
                                    |> Expect.equal []
                        )
            )
            Sprites.catalogue
        )
