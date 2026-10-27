module Chapter.Hall.Tiles exposing (Tile(..), fromChar, isWalkable, size, view)

{-| The building blocks of the hall map and how each one is drawn.
Every tile is 16×16 pixels.
-}

import Svg exposing (Svg)
import Svg.Attributes as A


type Tile
    = Floor
    | Start
    | Wall
    | Door
    | Curtain
    | CurtainFront
    | GlassTable
    | Pillar
    | GoldenKey
    | Portal Int


size : Int
size =
    16


{-| The maps are written as text in `Chapter.Hall.Map`; this is the legend.
A digit is a door that leads to the other door with the same digit.
-}
fromChar : Char -> Tile
fromChar char =
    case char of
        '.' ->
            Floor

        'S' ->
            Start

        'D' ->
            Door

        'C' ->
            Curtain

        'c' ->
            CurtainFront

        'T' ->
            GlassTable

        'P' ->
            Pillar

        'K' ->
            GoldenKey

        _ ->
            if Char.isDigit char then
                Portal (Char.toCode char - Char.toCode '0')

            else
                Wall


isWalkable : Tile -> Bool
isWalkable tile =
    case tile of
        Floor ->
            True

        Start ->
            True

        CurtainFront ->
            True

        GoldenKey ->
            True

        Portal _ ->
            True

        _ ->
            False


view : { x : Int, y : Int } -> Tile -> Svg msg
view position tile =
    let
        parts =
            case tile of
                Wall ->
                    wall position

                Door ->
                    wall position ++ door

                Curtain ->
                    wall position ++ curtain

                Portal number ->
                    wall position ++ portal number

                GlassTable ->
                    floor position ++ glass

                Pillar ->
                    floor position ++ pillar

                _ ->
                    floor position
    in
    Svg.g [ A.transform ("translate(" ++ String.fromInt (position.x * size) ++ " " ++ String.fromInt (position.y * size) ++ ")") ]
        parts



-- DRAWING (coordinates inside one 16×16 tile)


pixel : Int -> Int -> Int -> Int -> String -> Svg msg
pixel x y w h color =
    Svg.rect
        [ A.x (String.fromInt x)
        , A.y (String.fromInt y)
        , A.width (String.fromInt w)
        , A.height (String.fromInt h)
        , A.fill color
        ]
        []


floor : { x : Int, y : Int } -> List (Svg msg)
floor { x, y } =
    let
        checker =
            if modBy 2 (x + y) == 0 then
                "#3c2a4b"

            else
                "#33223f"
    in
    [ pixel 0 0 16 16 checker ]


wall : { x : Int, y : Int } -> List (Svg msg)
wall { y } =
    let
        offset =
            if modBy 2 y == 0 then
                0

            else
                6
    in
    [ pixel 0 0 16 16 "#5a3d73"
    , pixel 0 5 16 1 "#4a3060"
    , pixel 0 11 16 1 "#4a3060"
    , pixel (4 + offset) 0 1 5 "#4a3060"
    , pixel (10 - offset) 6 1 5 "#4a3060"
    , pixel 7 12 1 4 "#4a3060"
    ]


door : List (Svg msg)
door =
    [ pixel 3 3 10 13 "#8a5a3c"
    , pixel 4 2 8 1 "#8a5a3c"
    , pixel 7 4 1 12 "#6b3f2a"
    , pixel 10 9 2 2 "#efba5d"
    ]


{-| A door that leads somewhere: the frame has the pair's colour, and the
number on it matches the partner door's.
-}
portal : Int -> List (Svg msg)
portal number =
    let
        colour =
            case modBy 4 number of
                1 ->
                    "#e2557a"

                2 ->
                    "#5fb3e6"

                3 ->
                    "#7fc46a"

                _ ->
                    "#efba5d"
    in
    [ pixel 2 1 12 15 colour
    , pixel 4 3 8 13 "#6b3f2a"
    , pixel 5 4 6 6 "#f4ead2"
    , Svg.text_ [ A.x "8", A.y "9.5", A.class "door-number", A.textAnchor "middle" ] [ Svg.text (String.fromInt number) ]
    , pixel 10 11 1 2 colour
    ]


curtain : List (Svg msg)
curtain =
    [ pixel 2 1 12 15 "#b8435a"
    , pixel 5 1 1 15 "#8e2f45"
    , pixel 10 1 1 15 "#8e2f45"
    , pixel 2 1 12 1 "#efba5d"
    ]


glass : List (Svg msg)
glass =
    [ Svg.rect [ A.x "0", A.y "2", A.width "16", A.height "12", A.fill "#bfe6f0", A.fillOpacity "0.38" ] []
    , pixel 0 2 16 1 "#bfe6f0"
    , pixel 2 4 3 1 "#d7f1f7"
    ]


pillar : List (Svg msg)
pillar =
    [ pixel 2 0 12 16 "#8c7fa8"
    , pixel 10 0 4 16 "#6d6088"
    , pixel 3 0 2 16 "#b3a7cc"
    ]
