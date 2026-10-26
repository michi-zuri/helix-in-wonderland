module Pixel.Shapes exposing (Rect, layers, view)

{-| Many small rectangles drawn as a few SVG paths: one path per colour.

A browser handles one path with a hundred little squares much faster than a
hundred separate rectangles, which matters on old computers. Build the
layers once (when a picture is defined), then draw them as often as needed.

-}

import Dict
import Svg exposing (Svg)
import Svg.Attributes as A


type alias Rect =
    { x : Float, y : Float, width : Float, height : Float, colour : String }


{-| Group rectangles by colour into path data, keeping the colours in the
order they first appear (later colours are drawn on top).
-}
layers : List Rect -> List ( String, String )
layers rects =
    let
        order =
            rects
                |> List.foldl
                    (\rect seen ->
                        if List.member rect.colour seen then
                            seen

                        else
                            seen ++ [ rect.colour ]
                    )
                    []

        pieces =
            List.foldl
                (\rect byColour ->
                    Dict.update rect.colour
                        (\existing -> Just (piece rect :: Maybe.withDefault [] existing))
                        byColour
                )
                Dict.empty
                rects
    in
    List.map
        (\colour -> ( colour, Dict.get colour pieces |> Maybe.withDefault [] |> List.reverse |> String.concat ))
        order


{-| One rectangle as path commands: move to its corner, draw around it.
-}
piece : Rect -> String
piece { x, y, width, height } =
    "M"
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y
        ++ "h"
        ++ String.fromFloat width
        ++ "v"
        ++ String.fromFloat height
        ++ "h"
        ++ String.fromFloat (negate width)
        ++ "z"


view : List ( String, String ) -> Svg msg
view pathLayers =
    Svg.g [] (List.map (\( colour, d ) -> Svg.path [ A.d d, A.fill colour ] []) pathLayers)
