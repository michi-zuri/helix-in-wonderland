module Chapter.RabbitHole.Well exposing (background, heap, jar, strawBurst)

{-| The art of chapter 1: the walls of the rabbit hole sliding upwards as
Alice falls, with cupboards, maps and shelves on them as in the book, and
the jars she catches.

Positions are in the 320×192 scene. `scroll` is how far Alice has fallen,
in pixels; everything on the walls moves up by that much.

-}

import Pixel.Shapes as Shapes
import Svg exposing (Svg)
import Svg.Attributes as A
import Svg.Lazy


wallWidth : Float
wallWidth =
    60


{-| The well: a dark shaft between two brick walls, dust rushing past.
-}
background : Float -> List (Svg msg)
background scroll =
    [ rect 0 0 320 192 "#160c1d"
    , shaftBands scroll
    , dust scroll
    , walls scroll
    ]
        ++ List.concatMap (decorations scroll) [ LeftWall, RightWall ]
        ++ [ rect (wallWidth - 4) 0 4 192 "#2a1838"
           , rect (320 - wallWidth) 0 4 192 "#2a1838"
           ]


{-| A jar with a letter on its label. The highlighted one is the jar to
catch next.
-}
jar : { letter : Char, x : Float, y : Float, size : Float, highlighted : Bool } -> Svg msg
jar { letter, x, y, size, highlighted } =
    Svg.g [ A.transform ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ") scale(" ++ String.fromFloat size ++ ")") ]
        [ Svg.Lazy.lazy2 jarPicture letter highlighted ]


{-| The jar itself, drawn once per letter and moved as a whole.
-}
jarPicture : Char -> Bool -> Svg msg
jarPicture letter highlighted =
    Svg.g []
        ((if highlighted then
            [ rect -2 2 22 20 "#efba5d", rect -1 3 20 18 "#160c1d" ]

          else
            []
         )
            ++ [ rect 3 0 12 3 "#8a5a3c"
               , rect 3 3 12 1 "#6b3f2a"
               , Svg.rect [ A.y "4", A.width "18", A.height "16", A.fill "#a4a0e8", A.fillOpacity "0.35" ] []
               , rect 0 4 1 16 "#dbbfef"
               , rect 17 4 1 16 "#dbbfef"
               , rect 0 19 18 1 "#dbbfef"
               , rect 2 6 1 4 "#dbbfef"
               , rect 3 7 12 11 "#f4ead2"
               , Svg.text_ [ A.x "9", A.y "16", A.class "jar-letter", A.textAnchor "middle" ] [ Svg.text (String.fromChar letter) ]
               ]
        )



-- THE GROUND
--
-- "Thump! thump! down she came upon a heap of sticks and dry leaves, and the
-- fall was over."


{-| The heap at the bottom of the well, its top edge at `top`. The heap is
the same picture every time, so its paths are built once and only moved.
-}
heap : Float -> Svg msg
heap top =
    Svg.g [ A.transform ("translate(0 " ++ String.fromFloat top ++ ")") ]
        [ Svg.Lazy.lazy Shapes.view heapLayers ]


heapRows : Int
heapRows =
    24


heapLayers : List ( String, String )
heapLayers =
    Shapes.layers (List.concatMap heapRow (List.range 0 (heapRows - 1)) ++ sticks ++ leaves)


{-| One 2-pixel row of straw: narrow at the top of the mound, wide below.
-}
heapRow : Int -> List Shapes.Rect
heapRow row =
    let
        halfWidth =
            30 + 68 * sqrt (toFloat row / toFloat (heapRows - 1))

        left =
            160 - halfWidth

        chunks =
            ceiling (2 * halfWidth / 4)
    in
    List.map
        (\chunk -> { x = left + toFloat chunk * 4, y = toFloat (row * 2), width = 4, height = 2, colour = strawColour (row * 31 + chunk) })
        (List.range 0 (chunks - 1))


sticks : List Shapes.Rect
sticks =
    List.map
        (\i ->
            { x = 110 + toFloat (modBy 90 (hash (i * 11)))
            , y = toFloat ((4 + modBy 18 (hash (i * 5))) * 2)
            , width = toFloat (8 + modBy 8 (hash i))
            , height = 1
            , colour = "#7a4b33"
            }
        )
        (List.range 0 6)


leaves : List Shapes.Rect
leaves =
    List.map
        (\i ->
            { x = 100 + toFloat (modBy 120 (hash (i * 17)))
            , y = toFloat (6 + modBy 36 (hash (i * 23)))
            , width = 2
            , height = 2
            , colour =
                case modBy 3 (hash (i * 3)) of
                    0 ->
                        "#d9682b"

                    1 ->
                        "#a8452a"

                    _ ->
                        "#7d9a3a"
            }
        )
        (List.range 0 11)


strawColour : Int -> String
strawColour seed =
    case modBy 4 (hash seed) of
        0 ->
            "#e8bf55"

        1 ->
            "#d4a23c"

        2 ->
            "#f3d27a"

        _ ->
            "#c08a2e"


{-| Bits of straw flying up when Alice lands, `age` milliseconds after the
impact at (x, y). Nothing before the impact or after a second.
-}
strawBurst : { x : Float, y : Float, age : Float } -> List (Svg msg)
strawBurst { x, y, age } =
    if age < 0 || age > 1000 then
        []

    else
        let
            t =
                age / 1000
        in
        List.map
            (\i ->
                let
                    angle =
                        pi * (0.15 + 0.7 * toFloat (modBy 100 (hash (i * 7))) / 100)

                    speed =
                        50 + toFloat (modBy 60 (hash (i * 13)))
                in
                Svg.rect
                    [ A.x (String.fromFloat (x + cos angle * speed * t))
                    , A.y (String.fromFloat (y - sin angle * speed * t + 140 * t * t))
                    , A.width "3"
                    , A.height "1"
                    , A.fill (strawColour i)
                    , A.fillOpacity (String.fromFloat (1 - t))
                    ]
                    []
            )
            (List.range 0 15)



-- THE SHAFT
-- Everything on the walls and in the shaft is drawn once and then slid up
-- with a single transform per frame. (Sliding SVG patterns would be simpler,
-- but browsers redraw a pattern's tile every frame, which made old computers
-- stutter.) Each strip is one repeat taller than the screen, so sliding it by
-- up to one repeat always covers the view.


{-| Slide a picture up by `offset`, wrapped to its repeat height.
-}
sliding : Float -> Float -> Svg msg -> Svg msg
sliding repeat offset picture =
    Svg.g [ A.transform ("translate(0 " ++ String.fromFloat (negate (wrap repeat offset)) ++ ")") ] [ picture ]


{-| Both brick walls, sliding at the speed of the fall.
-}
walls : Float -> Svg msg
walls scroll =
    sliding 16 scroll (Svg.Lazy.lazy Shapes.view wallLayers)


wallLayers : List ( String, String )
wallLayers =
    let
        rows =
            List.range 0 13

        wall left =
            { x = left, y = 0, width = wallWidth, height = 224, colour = "#43295a" }
                :: List.concatMap
                    (\row ->
                        let
                            top =
                                toFloat (row * 16)

                            mortar x y w h =
                                { x = left + x, y = top + y, width = w, height = h, colour = "#382149" }
                        in
                        mortar 0 0 wallWidth 1
                            :: mortar 0 8 wallWidth 1
                            :: List.concatMap
                                (\column -> [ mortar (toFloat (column * 16)) 0 1 8, mortar (toFloat (column * 16 + 8)) 8 1 8 ])
                                (List.range 0 3)
                    )
                    rows
    in
    Shapes.layers (wall 0 ++ wall (320 - wallWidth))


{-| Darker and lighter bands down the shaft, sliding up at half speed.
-}
shaftBands : Float -> Svg msg
shaftBands scroll =
    sliding 80 (scroll * 0.5) (Svg.Lazy.lazy Shapes.view bandLayers)


bandLayers : List ( String, String )
bandLayers =
    Shapes.layers
        (List.map
            (\index ->
                { x = wallWidth
                , y = toFloat (index * 40)
                , width = 320 - 2 * wallWidth
                , height = 40
                , colour =
                    if modBy 2 index == 0 then
                        "#190d22"

                    else
                        "#1d1027"
                }
            )
            (List.range 0 6)
        )


{-| Specks of dust rushing past, faster than the walls. Their colour is
the light lilac already mixed with the dark shaft, because drawing solid
colour is cheaper than see-through.
-}
dust : Float -> Svg msg
dust scroll =
    sliding 220 (scroll * 1.7) (Svg.Lazy.lazy Shapes.view dustLayers)


dustLayers : List ( String, String )
dustLayers =
    Shapes.layers
        (List.concatMap
            (\index ->
                let
                    speck =
                        { x = wallWidth + 2 + toFloat (modBy 196 (hash (index + 7)))
                        , y = toFloat (modBy 220 (hash (index * 13)))
                        , width = 1
                        , height = 6
                        , colour = "#4a3a56"
                        }
                in
                -- Each speck twice, one repeat apart, so the strip is seamless.
                [ speck, { speck | y = speck.y + 220 } ]
            )
            (List.range 0 25)
        )



-- THINGS ON THE WALLS
--
-- "She looked at the sides of the well, and noticed that they were filled
-- with cupboards and book-shelves; here and there she saw maps and pictures
-- hung upon pegs." Which thing hangs where is decided by a hash of its
-- position, so the walls look the same every time.


type Wall
    = LeftWall
    | RightWall


type Decoration
    = Shelf
    | Map
    | Picture
    | Cupboard
    | BareWall


decorationSpacing : Int
decorationSpacing =
    56


{-| The things on one wall that are in view. Each is drawn once, at its own
origin, and slid into place with a single transform.
-}
decorations : Float -> Wall -> List (Svg msg)
decorations scroll wall =
    let
        firstSlot =
            floor (scroll / toFloat decorationSpacing) - 1

        ( x, wallIndex ) =
            case wall of
                LeftWall ->
                    ( 10, 0 )

                RightWall ->
                    ( 274, 1 )
    in
    List.range firstSlot (firstSlot + 5)
        |> List.map
            (\slot ->
                Svg.g
                    [ A.transform
                        ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat (toFloat (slot * decorationSpacing) - scroll + 8) ++ ")")
                    ]
                    [ Svg.Lazy.lazy decorationPicture (decorationAt (slot * 2 + wallIndex)) ]
            )


decorationAt : Int -> Decoration
decorationAt seed =
    case modBy 5 (hash seed) of
        0 ->
            Shelf

        1 ->
            Map

        2 ->
            Picture

        3 ->
            Cupboard

        _ ->
            BareWall


decorationPicture : Decoration -> Svg msg
decorationPicture kind =
    Svg.g [] (decoration kind 0 0)


decoration : Decoration -> Float -> Float -> List (Svg msg)
decoration kind x y =
    case kind of
        Shelf ->
            [ rect (x - 4) (y + 30) 42 3 "#7a4b33", rect (x - 4) (y + 33) 42 1 "#5a3424" ]
                ++ List.concatMap
                    (\( dx, color ) ->
                        [ rect (x + dx + 1) (y + 20) 6 2 "#8a5a3c"
                        , Svg.rect [ A.x (String.fromFloat (x + dx)), A.y (String.fromFloat (y + 22)), A.width "8", A.height "8", A.fill color, A.fillOpacity "0.75" ] []
                        , rect (x + dx + 1) (y + 24) 6 3 "#f4ead2"
                        ]
                    )
                    -- The middle one is orange: "ORANGE MARMALADE", but empty.
                    [ ( 0, "#a4a0e8" ), ( 12, "#ef9a3c" ), ( 24, "#9ff28f" ) ]

        Map ->
            [ rect x (y + 6) 34 24 "#e9dcb8"
            , rect (x + 3) (y + 11) 14 1 "#b49a6a"
            , rect (x + 8) (y + 16) 20 1 "#b49a6a"
            , rect (x + 4) (y + 22) 12 1 "#b49a6a"
            , rect (x + 20) (y + 9) 1 16 "#b49a6a"
            , rect (x + 24) (y + 19) 3 3 "#c8344e"
            ]

        Picture ->
            [ rect (x + 2) (y + 4) 28 22 "#efba5d"
            , rect (x + 5) (y + 7) 22 10 "#5fb3e6"
            , rect (x + 5) (y + 17) 22 6 "#6fae62"
            , rect (x + 18) (y + 9) 4 2 "#f4f0ff"
            ]

        Cupboard ->
            [ rect x y 36 44 "#6b3f2a"
            , rect (x + 3) (y + 3) 14 38 "#4d2c1d"
            , rect (x + 19) (y + 3) 14 38 "#4d2c1d"
            , rect (x + 5) (y + 5) 10 34 "#6b3f2a"
            , rect (x + 21) (y + 5) 10 34 "#6b3f2a"
            , rect (x + 15) (y + 22) 2 2 "#efba5d"
            , rect (x + 19) (y + 22) 2 2 "#efba5d"
            ]

        BareWall ->
            []



-- HELPERS


rect : Float -> Float -> Float -> Float -> String -> Svg msg
rect x y w h color =
    Svg.rect
        [ A.x (String.fromFloat x)
        , A.y (String.fromFloat y)
        , A.width (String.fromFloat w)
        , A.height (String.fromFloat h)
        , A.fill color
        ]
        []


{-| `value` wrapped into 0 ≤ result < `length`, also for negative values.
-}
wrap : Float -> Float -> Float
wrap length value =
    value - length * toFloat (floor (value / length))


{-| A cheap, repeatable scramble of an integer (Park–Miller). Good enough to
make the walls look random without being random.
-}
hash : Int -> Int
hash n =
    let
        prime =
            2147483647

        once x =
            modBy prime (x * 48271)
    in
    once (once (abs n + 1))
