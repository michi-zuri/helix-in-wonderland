module KeyboardLayoutTest exposing (suite)

import Expect
import Keyboard.Layout as Layout exposing (Layout(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Keyboard.Layout"
        [ describe "home row"
            [ test "QWERTY rests on asdf and jkl;" <|
                \_ -> Layout.homeKeys UsQwerty |> Expect.equal { left = [ 'a', 's', 'd', 'f' ], right = [ 'j', 'k', 'l', ';' ] }
            , test "Swiss QWERTZ rests on asdf and jklö" <|
                \_ -> Layout.homeKeys SwissQwertz |> Expect.equal { left = [ 'a', 's', 'd', 'f' ], right = [ 'j', 'k', 'l', 'ö' ] }
            , test "AZERTY rests on qsdf and jklm" <|
                \_ -> Layout.homeKeys FrenchAzerty |> Expect.equal { left = [ 'q', 's', 'd', 'f' ], right = [ 'j', 'k', 'l', 'm' ] }
            , test "the stretch keys are g and h everywhere" <|
                \_ -> List.map Layout.stretchKeys Layout.all |> Expect.equal (List.repeat 4 [ 'g', 'h' ])
            ]
        , describe "detecting the layout from a key press"
            [ test "z typed by the physical Y key means QWERTZ" <|
                \_ -> Layout.detect UsQwerty { code = "KeyY", key = "z" } |> Expect.equal (Just GermanQwertz)
            , test "a Swiss player is not switched to German by z" <|
                \_ -> Layout.detect SwissQwertz { code = "KeyY", key = "z" } |> Expect.equal Nothing
            , test "$ right of the home row means Swiss" <|
                \_ -> Layout.detect GermanQwertz { code = "Backslash", key = "$" } |> Expect.equal (Just SwissQwertz)
            , test "ß means German" <|
                \_ -> Layout.detect SwissQwertz { code = "Minus", key = "ß" } |> Expect.equal (Just GermanQwertz)
            , test "a typed by the physical Q key means AZERTY" <|
                \_ -> Layout.detect GermanQwertz { code = "KeyQ", key = "a" } |> Expect.equal (Just FrenchAzerty)
            , test "a correct setting is left alone" <|
                \_ -> Layout.detect UsQwerty { code = "KeyZ", key = "z" } |> Expect.equal Nothing
            , test "ordinary letters say nothing about the layout" <|
                \_ -> Layout.detect FrenchAzerty { code = "KeyJ", key = "j" } |> Expect.equal Nothing
            ]
        ]
