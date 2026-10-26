module Chapter.RabbitHole.Well exposing (background, heap, jar, strawBurst)

{-| The art of chapter 1: the walls of the rabbit hole sliding upwards as
Alice falls, with cupboards, maps and shelves on them as in the book, and
the jars she catches.

Positions are in the 320×192 scene. `scroll` is how far Alice has fallen,
in pixels; everything on the walls moves up by that much.

-}

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
    rect 0 0 320 192 "#160c1d"
        :: List.map (shaftBand scroll) (List.range 0 6)
        ++ List.map (dust scroll) (List.range 0 25)
        ++ [ Svg.defs [] [ bricks scroll ]
           , Svg.rect [ A.width (String.fromFloat wallWidth), A.height "192", A.fill "url(#well-bricks)" ] []
           , Svg.rect [ A.x (String.fromFloat (320 - wallWidth)), A.width (String.fromFloat wallWidth), A.height "192", A.fill "url(#well-bricks)" ] []
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
the same picture every time, so it is built once and only moved.
-}
heap : Float -> Svg msg
heap top =
    Svg.g [ A.transform ("translate(0 " ++ String.fromFloat top ++ ")") ]
        [ Svg.Lazy.lazy heapPicture () ]


heapRows : Int
heapRows =
    24


heapPicture : () -> Svg msg
heapPicture () =
    Svg.g [] (List.concatMap heapRow (List.range 0 (heapRows - 1)) ++ sticks ++ leaves)


{-| One 2-pixel row of straw: narrow at the top of the mound, wide below.
-}
heapRow : Int -> List (Svg msg)
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
        (\chunk -> rect (left + toFloat chunk * 4) (toFloat (row * 2)) 4 2 (strawColour (row * 31 + chunk)))
        (List.range 0 (chunks - 1))


sticks : List (Svg msg)
sticks =
    List.map
        (\i ->
            let
                row =
                    4 + modBy 18 (hash (i * 5))
            in
            rect (110 + toFloat (modBy 90 (hash (i * 11)))) (toFloat (row * 2)) (toFloat (8 + modBy 8 (hash i))) 1 "#7a4b33"
        )
        (List.range 0 6)


leaves : List (Svg msg)
leaves =
    List.map
        (\i ->
            let
                colour =
                    case modBy 3 (hash (i * 3)) of
                        0 ->
                            "#d9682b"

                        1 ->
                            "#a8452a"

                        _ ->
                            "#7d9a3a"
            in
            rect (100 + toFloat (modBy 120 (hash (i * 17)))) (toFloat (6 + modBy 36 (hash (i * 23)))) 2 2 colour
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


shaftBand : Float -> Int -> Svg msg
shaftBand scroll index =
    let
        y =
            wrap 280 (toFloat (index * 40) - scroll * 0.5) - 40

        color =
            if modBy 2 index == 0 then
                "#190d22"

            else
                "#1d1027"
    in
    rect wallWidth y (320 - 2 * wallWidth) 40 color


dust : Float -> Int -> Svg msg
dust scroll index =
    let
        x =
            wallWidth + 2 + toFloat (modBy 196 (hash (index + 7)))

        y =
            wrap 220 (toFloat (modBy 400 (hash (index * 13))) - scroll * 1.7) - 14
    in
    Svg.rect [ A.x (String.fromFloat x), A.y (String.fromFloat y), A.width "1", A.height "6", A.fill "#dbbfef", A.fillOpacity "0.22" ] []


{-| One 16×16 tile of brickwork, repeated over both walls and shifted up
as Alice falls.
-}
bricks : Float -> Svg msg
bricks scroll =
    Svg.pattern
        [ A.id "well-bricks"
        , A.width "16"
        , A.height "16"
        , A.patternUnits "userSpaceOnUse"
        , A.patternTransform ("translate(0 " ++ String.fromFloat (negate (wrap 16 scroll)) ++ ")")
        ]
        [ rect 0 0 16 16 "#43295a"
        , rect 0 0 16 1 "#382149"
        , rect 0 8 16 1 "#382149"
        , rect 0 0 1 8 "#382149"
        , rect 8 8 1 8 "#382149"
        ]



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
        |> List.concatMap
            (\slot ->
                decoration (decorationAt (slot * 2 + wallIndex))
                    x
                    (toFloat (slot * decorationSpacing) - scroll + 8)
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
