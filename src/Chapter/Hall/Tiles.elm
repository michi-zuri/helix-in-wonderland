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
    | Portal
    | Dropped


size : Int
size =
    16


{-| The maps are written as text in `Chapter.Hall.Map`; this is the legend.
`O` is a door that leads to another `O` door (see `Chapter.Hall.Doors`);
`x` is something dropped on the floor.
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

        'x' ->
            Dropped

        'O' ->
            Portal

        _ ->
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

        Portal ->
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

                -- Looks like any other door: where it leads is a surprise.
                Portal ->
                    wall position ++ door

                Dropped ->
                    floor position ++ dropped position

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


{-| Something dropped on the floor, in Alice's hurry or the Rabbit's. Which
thing lies where follows from the position, so the hall looks varied but
the same every time.
-}
dropped : { x : Int, y : Int } -> List (Svg msg)
dropped { x, y } =
    case modBy 6 (x * 7 + y * 3) of
        0 ->
            -- a pile of two books
            [ pixel 2 9 12 3 "#b8435a"
            , pixel 2 10 12 1 "#f4ead2"
            , pixel 3 5 10 4 "#4f7fbf"
            , pixel 3 6 10 1 "#f4ead2"
            , pixel 3 12 12 1 "#2a1c35"
            ]

        1 ->
            -- the White Rabbit's white kid gloves
            [ pixel 2 6 5 7 "#f4ead2"
            , pixel 2 4 1 3 "#f4ead2"
            , pixel 4 3 1 3 "#f4ead2"
            , pixel 6 4 1 3 "#f4ead2"
            , pixel 9 7 5 7 "#e3d6bb"
            , pixel 9 5 1 3 "#e3d6bb"
            , pixel 11 4 1 3 "#e3d6bb"
            , pixel 13 5 1 3 "#e3d6bb"
            ]

        2 ->
            -- the Rabbit's fan
            [ pixel 7 11 2 3 "#6b3f2a"
            , pixel 5 8 6 3 "#e2557a"
            , pixel 3 5 10 3 "#e2557a"
            , pixel 3 5 10 1 "#f4a6bb"
            , pixel 7 5 1 6 "#b8435a"
            ]

        3 ->
            -- a teacup on its saucer
            [ pixel 2 12 12 2 "#d7f1f7"
            , pixel 4 6 8 6 "#f4ead2"
            , pixel 4 7 8 1 "#5fb3e6"
            , pixel 12 8 2 3 "#f4ead2"
            , pixel 5 5 6 1 "#8a5a3c"
            ]

        4 ->
            -- the little cake box marked EAT ME
            [ pixel 3 6 10 8 "#efba5d"
            , pixel 3 6 10 2 "#c98f3a"
            , pixel 5 10 6 1 "#b8435a"
            , pixel 5 12 4 1 "#b8435a"
            ]

        _ ->
            -- a jar of orange marmalade
            [ pixel 4 6 8 8 "#e8873a"
            , pixel 4 4 8 2 "#f4ead2"
            , pixel 5 8 6 3 "#f4ead2"
            , pixel 5 3 6 1 "#b8435a"
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
