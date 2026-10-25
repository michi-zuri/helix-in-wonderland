module Pixel.Sprites exposing (alice, aliceFalling, aliceStepping, aliceSwimming, caterpillar, goldenKey, mouse, tart, whiteRabbit)

{-| The game's characters and props. Original pixel art, in the spirit of the
1865 book rather than any film.
-}

import Pixel.Sprite as Sprite exposing (Sprite)


alicePalette : List ( Char, String )
alicePalette =
    [ ( 'k', "#1a1020" ) -- headband, eyes, shoes
    , ( 'y', "#f2d16b" ) -- hair
    , ( 'Y', "#c99a3b" ) -- hair shadow
    , ( 's', "#f6c9a6" ) -- skin
    , ( 'p', "#e88aa8" ) -- mouth
    , ( 'b', "#6fa8dc" ) -- dress
    , ( 'B', "#3d6fa8" ) -- dress hem
    , ( 'w', "#f4f0ff" ) -- apron
    ]


aliceUpperBody : List String
aliceUpperBody =
    [ "....kkkk...."
    , "...yyyyyy..."
    , "..yyyyyyyy.."
    , "..yssssssy.."
    , "..ysksskys.."
    , "..yssssssy.."
    , "..yYsppsYy.."
    , "...bbwwbb..."
    , "..bbbwwbbb.."
    , ".sbbwwwwbbs."
    , ".s.bwwwwb.s."
    , "...bwwwwb..."
    , "..bbwwwwbb.."
    , "..BBBBBBBB.."
    ]


alice : Sprite
alice =
    Sprite.fromRows alicePalette
        (aliceUpperBody
            ++ [ "....s..s...."
               , "....k..k...."
               ]
        )


{-| Second walking frame: legs apart.
-}
aliceStepping : Sprite
aliceStepping =
    Sprite.fromRows alicePalette
        (aliceUpperBody
            ++ [ "...s....s..."
               , "...k....k..."
               ]
        )


{-| Arms up, dress puffed out, falling down the rabbit hole.
-}
aliceFalling : Sprite
aliceFalling =
    Sprite.fromRows alicePalette
        [ "s...kkkk...s"
        , "s..yyyyyy..s"
        , "syyyyyyyyyys"
        , ".yssssssssy."
        , "..ysksskys.."
        , "..yssssssy.."
        , "..yYsppsYy.."
        , "...bbwwbb..."
        , "..bbbwwbbb.."
        , ".bbbwwwwbbb."
        , "bbbbwwwwbbbb"
        , "BBBBBBBBBBBB"
        , "...s....s..."
        , "..s......s.."
        , ".k........k."
        ]


{-| Just the head and shoulders, for swimming in the pool of tears.
-}
aliceSwimming : Sprite
aliceSwimming =
    Sprite.fromRows alicePalette (List.take 8 aliceUpperBody)


whiteRabbit : Sprite
whiteRabbit =
    Sprite.fromRows
        [ ( 'w', "#f4f0ff" ) -- fur
        , ( 'p', "#f4a6c0" ) -- inner ears, nose
        , ( 'k', "#ff5d8f" ) -- pink eyes, as in the book
        , ( 'r', "#c8344e" ) -- waistcoat
        , ( 'y', "#efba5d" ) -- pocket watch
        ]
        [ "...ww..ww..."
        , "...wp..wp..."
        , "...wp..wp..."
        , "...ww..ww..."
        , "..wwwwwwww.."
        , "..wkwwwwkw.."
        , "..wwwppwww.."
        , "...wwwwww..."
        , "..rrrrrrrr.."
        , ".wrrryyrrrw."
        , ".w.rrrrrr.w."
        , "...rrrrrr..."
        , "...wwwwww..."
        , "...ww..ww..."
        , "...ww..ww..."
        , "..www..www.."
        ]


{-| The large blue Caterpillar on its mushroom, arms folded.
-}
caterpillar : Sprite
caterpillar =
    Sprite.fromRows
        [ ( 'c', "#5fb3e6" ) -- body
        , ( 'C', "#3d86b8" ) -- folded arms
        , ( 'k', "#120a18" ) -- eye
        , ( 'r', "#c8344e" ) -- mushroom cap
        , ( 'w', "#f4f0ff" ) -- spots
        , ( 't', "#efe2c8" ) -- stem
        ]
        [ "................"
        , ".....cc........."
        , "....cccc........"
        , "....ckcc........"
        , ".....ccc........"
        , "....cCcc........"
        , "...cCcc........."
        , "..rrrrrrrrrrrr.."
        , ".rrwwrrrrrrwwrr."
        , "rrrrrrrwwrrrrrrr"
        , "rrwwrrrrrrrrwwrr"
        , ".rrrrrrrrrrrrrr."
        , "......tttt......"
        , "......tttt......"
        , "......tttt......"
        , ".....tttttt....."
        ]


{-| The Mouse, swimming in the pool of tears.
-}
mouse : Sprite
mouse =
    Sprite.fromRows
        [ ( 'g', "#9a86b0" ), ( 'k', "#120a18" ), ( 'p', "#e88aa8" ) ]
        [ "..g........"
        , ".ggg......."
        , "gkgggggg..."
        , "ggggggggg.."
        , ".gggggggggp"
        , "..g..g...pp"
        ]


goldenKey : Sprite
goldenKey =
    Sprite.fromRows [ ( 'y', "#efba5d" ) ]
        [ ".yy......."
        , "y..yyyyyyy"
        , ".yy....y.y"
        ]


{-| The Queen of Hearts' tarts are the game's stars: up to three per chapter.
A golden fluted crust, bright jam with a shine, and a heart on top.
-}
tart : Sprite
tart =
    Sprite.fromRows
        [ ( 'c', "#f4c15d" ) -- crust
        , ( 'C', "#d9952f" ) -- crust flutes
        , ( 'B', "#b9772a" ) -- crust underneath
        , ( 'j', "#e8203f" ) -- jam
        , ( 'w', "#ffe0e6" ) -- shine on the jam
        , ( 'p', "#ff9fc2" ) -- the heart
        ]
        [ "...cCcCcCcC..."
        , "..cjjjjjjjjc.."
        , ".cjwjppjppjjc."
        , "cjjjjpppppjjjc"
        , "cjjjjjpppjjjjc"
        , ".cjjjjjpjjjjc."
        , "..cjjjjjjjjc.."
        , "...BBBBBBBB..."
        ]
