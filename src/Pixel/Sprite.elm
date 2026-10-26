module Pixel.Sprite exposing (Sprite, fromRows, palette, rows, size, view, viewAt)

{-| Pixel art drawn from text, one character per pixel.

    fromRows [ ( 'k', "#1a1020" ), ( 'y', "#f2d16b" ) ]
        [ "..kk.."
        , ".yyyy."
        ]

`.` is transparent. When the sprite is defined, its pixels are turned into
one SVG path per colour (see `Pixel.Shapes`), so drawing it stays cheap
even on old computers, and drawing the same sprite again is skipped
entirely (`Svg.Lazy`).

-}

import Dict
import Pixel.Shapes as Shapes
import Svg exposing (Svg)
import Svg.Attributes as A
import Svg.Lazy


type Sprite
    = Sprite
        { width : Int
        , height : Int
        , layers : List ( String, String )
        , rows : List String
        , palette : List ( Char, String )
        }


type alias Run =
    { x : Int, y : Int, length : Int, color : String }


fromRows : List ( Char, String ) -> List String -> Sprite
fromRows colours pixelRows =
    let
        colors =
            Dict.fromList colours
    in
    Sprite
        { width = pixelRows |> List.map String.length |> List.maximum |> Maybe.withDefault 0
        , height = List.length pixelRows
        , layers =
            List.indexedMap (\y row -> rowRuns (\c -> Dict.get c colors) y row) pixelRows
                |> List.concat
                |> List.map (\run -> { x = toFloat run.x, y = toFloat run.y, width = toFloat run.length, height = 1, colour = run.color })
                |> Shapes.layers
        , rows = pixelRows
        , palette = colours
        }


{-| The picture as text, one string per row (used by the pixel editor).
-}
rows : Sprite -> List String
rows (Sprite s) =
    s.rows


{-| Which letter stands for which colour.
-}
palette : Sprite -> List ( Char, String )
palette (Sprite s) =
    s.palette


rowRuns : (Char -> Maybe String) -> Int -> String -> List Run
rowRuns colorOf y row =
    let
        step ( x, char ) runs =
            case ( colorOf char, runs ) of
                ( Just color, previous :: rest ) ->
                    if previous.color == color && previous.x + previous.length == x then
                        { previous | length = previous.length + 1 } :: rest

                    else
                        { x = x, y = y, length = 1, color = color } :: runs

                ( Just color, [] ) ->
                    [ { x = x, y = y, length = 1, color = color } ]

                ( Nothing, _ ) ->
                    runs
    in
    String.toList row
        |> List.indexedMap Tuple.pair
        |> List.foldl step []


size : Sprite -> { width : Int, height : Int }
size (Sprite s) =
    { width = s.width, height = s.height }


{-| Draw at the origin, one SVG unit per pixel.
-}
view : Sprite -> Svg msg
view sprite =
    Svg.Lazy.lazy viewLayers sprite


viewLayers : Sprite -> Svg msg
viewLayers (Sprite s) =
    Shapes.view s.layers


{-| Draw with the top-left corner at a position, scaled by `pixelSize`.
-}
viewAt : { x : Float, y : Float, pixelSize : Float } -> Sprite -> Svg msg
viewAt { x, y, pixelSize } sprite =
    Svg.g
        [ A.transform
            ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ") scale(" ++ String.fromFloat pixelSize ++ ")")
        ]
        [ view sprite ]
