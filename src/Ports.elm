port module Ports exposing (fullscreenChanged, playSound, saveProgress, saveSettings, setFullscreen)

{-| The only doors to JavaScript. The JS side lives in `public/index.html`.
-}

import Json.Encode as Encode


port playSound : String -> Cmd msg


port saveSettings : Encode.Value -> Cmd msg


port saveProgress : Encode.Value -> Cmd msg


{-| Ask the browser to enter (True) or leave (False) fullscreen.
-}
port setFullscreen : Bool -> Cmd msg


{-| The browser entered or left fullscreen, for example when the player held
Esc or pressed F11.
-}
port fullscreenChanged : (Bool -> msg) -> Sub msg
