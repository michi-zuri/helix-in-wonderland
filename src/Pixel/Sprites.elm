module Pixel.Sprites exposing (Entry, alice, aliceFalling, aliceStepping, aliceSwimming, catalogue, caterpillar, cheshireCat, goldenKey, mouse, tart, walkingFramesPerSecond, whiteRabbit)

{-| The game's characters and props. Original pixel art, in the spirit of the
1865 book rather than any film.

Every picture is written as text: one line per row of pixels, one letter
per pixel, and `.` for "nothing here". The list at the start of each
picture says which letter is which colour. See ART.md for how to propose
changes.

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
    , ( 'a', "#ff5743" ) -- hair ties (#130, #132)
    ]


aliceUpperBody : List String
aliceUpperBody =
    [ "....kkkk...."
    , "...yyyyyy..."
    , "..yyyyyyyy.."
    , "..yssssssy.."
    , "..ysksskys.."
    , "..assssssa.."
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


{-| Falling down the rabbit hole: pigtails with their hair ties flying
up, dress puffed out (#130).
-}
aliceFalling : Sprite
aliceFalling =
    Sprite.fromRows alicePalette
        [ "yy..kkkk...y"
        , ".yayyyyy.ayy"
        , ".ayyyyyyyya."
        , ".yssssssssy."
        , "..ysksskys.."
        , "..yssssssy.."
        , "bbyYsppsYy.."
        , ".bbbbwwbbbbb"
        , "..bbbwwbb..."
        , ".bbbwwwwbb.."
        , "bbbbwwwwbbb."
        , "BBBBBBBBBBB."
        , "...s...s...."
        , "..s.....s..."
        , ".kk.....kk.."
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


{-| The Mouse, swimming in the pool of tears (redrawn in #131).
-}
mouse : Sprite
mouse =
    Sprite.fromRows
        [ ( 'g', "#9a86b0" ), ( 'k', "#120a18" ), ( 'p', "#e88aa8" ) ]
        [ "...g.g....."
        , "..ggg......"
        , "ggkggg....."
        , ".ggggggg..p"
        , ".gggggggpp."
        , "..g..g....."
        ]


{-| The Cheshire Cat, who turns up only for a very particular player.
Purple stripes and the famous grin.
-}
cheshireCat : Sprite
cheshireCat =
    Sprite.fromRows
        [ ( 'p', "#9b5fc0" ), ( 's', "#6a3d8a" ), ( 'y', "#efba5d" ), ( 'k', "#120a18" ), ( 'w', "#f4ead2" ), ( 'r', "#e88aa8" ) ]
        [ ".p........p."
        , ".pp......pp."
        , ".pspppppsp.."
        , "pppppppppppp"
        , "psykppppkysp"
        , "pppppppppppp"
        , "pswwwwwwwwsp"
        , "ppwkwkwkwwpp"
        , ".pprrrrrrpp."
        , "..pspppsp..."
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


{-| A picture in the pixel editor: one frame, or several that take turns,
like Alice walking (#136).
-}
type alias Entry =
    { name : String
    , frames : List Sprite
    , framesPerSecond : Float
    }


{-| How fast Alice's walking frames take turns, as chapter 4 shows them.
-}
walkingFramesPerSecond : Float
walkingFramesPerSecond =
    8


{-| Every picture the pixel editor offers. Alice standing and stepping are
the two frames of `alice`.
-}
catalogue : List Entry
catalogue =
    let
        still name sprite =
            { name = name, frames = [ sprite ], framesPerSecond = 4 }
    in
    [ { name = "alice", frames = [ alice, aliceStepping ], framesPerSecond = walkingFramesPerSecond }
    , still "aliceFalling" aliceFalling
    , still "aliceSwimming" aliceSwimming
    , still "whiteRabbit" whiteRabbit
    , still "caterpillar" caterpillar
    , still "mouse" mouse
    , still "cheshireCat" cheshireCat
    , still "goldenKey" goldenKey
    , still "tart" tart
    ]
