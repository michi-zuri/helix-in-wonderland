module Main exposing (main)

{-| The page: everything the game does lives in `App`; this turns what it
asks for into real ports and browser subscriptions.
-}

import App
import Browser
import Browser.Events
import Json.Decode as Decode
import Ports


main : Program Decode.Value App.Model App.Msg
main =
    Browser.element
        { init = App.init >> perform
        , update = \msg model -> App.update msg model |> perform
        , view = App.view
        , subscriptions = subscriptions
        }


perform : ( App.Model, List App.Command ) -> ( App.Model, Cmd App.Msg )
perform ( model, commands ) =
    ( model, Cmd.batch (List.map toCmd commands) )


toCmd : App.Command -> Cmd App.Msg
toCmd command =
    case command of
        App.SaveSettings value ->
            Ports.saveSettings value

        App.SaveProgress value ->
            Ports.saveProgress value

        App.Sound name ->
            Ports.playSound name

        App.SetFullscreen on ->
            Ports.setFullscreen on


subscriptions : App.Model -> Sub App.Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown App.keyDecoder
        , Ports.fullscreenChanged App.fullscreenChanged
        , if App.needsFrames model then
            Browser.Events.onAnimationFrameDelta App.frame

          else
            Sub.none
        ]
