module Ui.Tarts exposing (view)

{-| A row of three tarts: earned ones in colour, the rest faded.
-}

import Html exposing (Html)
import Html.Attributes as A
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Svg
import Svg.Attributes as SA


view : String -> Int -> Html msg
view sizeClass earned =
    Html.span [ A.class "tarts", A.class sizeClass ]
        (List.map
            (\index ->
                Svg.svg
                    [ SA.viewBox "0 0 14 8"
                    , SA.class
                        (if index < earned then
                            "tart"

                         else
                            "tart dim"
                        )
                    ]
                    [ Sprite.view Sprites.tart ]
            )
            [ 0, 1, 2 ]
        )
