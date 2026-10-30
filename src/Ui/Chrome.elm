module Ui.Chrome exposing (header, statusLine)

{-| The frame around the game: settings bar on top, Helix status line below
the stage.
-}

import Chapter.Contract exposing (Status)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Keyboard.Layout as Layout exposing (Layout)
import Language exposing (Language)
import Settings exposing (Settings)
import Texts exposing (Texts)


header :
    { texts : Texts
    , settings : Settings
    , onLanguage : Language -> msg
    , onLayout : Layout -> msg
    , onToggleSound : msg
    , onMenu : msg
    , onFullscreen : msg
    }
    -> Html msg
header { texts, settings, onLanguage, onLayout, onToggleSound, onMenu, onFullscreen } =
    let
        choice : (a -> msg) -> (a -> String) -> a -> a -> Html msg
        choice toMsg label current option =
            Html.button
                [ A.attribute "aria-pressed"
                    (if option == current then
                        "true"

                     else
                        "false"
                    )
                , E.onClick (toMsg option)
                ]
                [ Html.text (label option) ]
    in
    Html.header [ A.class "bar" ]
        [ Html.div [ A.class "brand" ]
            [ Html.b [] [ Html.text "hx" ]
            , Html.text (" · " ++ texts.logo.before ++ " " ++ texts.logo.small ++ " " ++ texts.logo.after)
            ]
        , Html.div [ A.class "settings" ]
            [ Html.div [ A.class "segmented", A.attribute "role" "group", A.attribute "aria-label" "Language" ]
                (List.map (choice onLanguage Language.toCode settings.language) Language.all)
            , Html.div [ A.class "segmented", A.attribute "role" "group", A.attribute "aria-label" "Keyboard" ]
                (List.map (choice onLayout Layout.name settings.layout) Layout.all)
            , Html.button [ A.class "plain", E.onClick onToggleSound ]
                [ Html.text
                    (if settings.soundOn then
                        texts.soundOn

                     else
                        texts.soundOff
                    )
                ]
            , Html.button [ A.class "plain", E.onClick onMenu ] [ Html.text texts.menu ]
            , Html.button [ A.class "plain", E.onClick onFullscreen ] [ Html.text ("⛶ " ++ texts.fullscreen) ]
            ]
        ]


{-| In fullscreen, `exit` adds a small button at the end to leave it,
since the settings bar is hidden then.
-}
statusLine : Maybe { onExit : msg, label : String } -> Status -> Html msg
statusLine exit status =
    Html.div [ A.class "status-line" ]
        ([ Html.span
            [ A.class "status-mode"
            , A.classList [ ( "insert", status.mode == "INS" ) ]
            ]
            [ Html.text status.mode ]
         , Html.span [ A.class "status-file" ] [ Html.text status.file ]
         , Html.span [ A.class "status-gap" ] []
         , Html.span [ A.class "status-keys" ] [ Html.text status.pendingKeys ]
         , Html.span [ A.class "status-position" ] [ Html.text status.position ]
         ]
            ++ (case exit of
                    Just { onExit, label } ->
                        [ Html.button [ A.class "status-exit", E.onClick onExit, A.title label, A.attribute "aria-label" label ] [ Html.text "✕" ] ]

                    Nothing ->
                        []
               )
        )
