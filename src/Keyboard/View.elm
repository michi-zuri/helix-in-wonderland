module Keyboard.View exposing (view)

{-| The on-screen keyboard. Every key is tinted with the colour of the finger
that presses it, so the picture doubles as a ten-finger chart.

The game redraws 60 times a second, but the keyboard changes only when the
next key, the outlined keys or a key flash change. `view` hands those to
`Html.Lazy` as plain strings, so all other frames skip the keyboard.

-}

import Chapter.Contract exposing (KeyboardFocus)
import Html exposing (Html)
import Html.Attributes as A
import Html.Lazy
import Keyboard.Layout as Layout exposing (Finger, Layout)
import Texts exposing (Texts)


type alias Flash =
    { key : String, ok : Bool }


view : { texts : Texts, layout : Layout, focus : KeyboardFocus, flash : Maybe Flash } -> Html msg
view { texts, layout, focus, flash } =
    Html.Lazy.lazy5 viewKeyboard
        texts
        layout
        (focus.target |> Maybe.map String.fromChar |> Maybe.withDefault "")
        (String.join "\n" focus.helpers)
        (case flash of
            Just f ->
                f.key
                    ++ (if f.ok then
                            "\n+"

                        else
                            "\n-"
                       )

            Nothing ->
                ""
        )


{-| The arguments come back from their string forms here.
-}
viewKeyboard : Texts -> Layout -> String -> String -> String -> Html msg
viewKeyboard texts layout targetKey helperKeys flashKey =
    let
        focus =
            { target = String.toList targetKey |> List.head
            , helpers = String.split "\n" helperKeys
            }

        flash =
            case String.split "\n" flashKey of
                [ flashed, "+" ] ->
                    Just { key = flashed, ok = True }

                [ flashed, "-" ] ->
                    Just { key = flashed, ok = False }

                _ ->
                    Nothing

        key extraClass keyName label maybeFinger =
            Html.div
                [ A.class "key"
                , A.attribute "data-key" keyName
                , A.class extraClass
                , A.class (maybeFinger |> Maybe.map fingerClass |> Maybe.withDefault "f8")
                , A.classList
                    [ ( "helper", List.member keyName focus.helpers )
                    , ( "target", Maybe.map String.fromChar focus.target == Just keyName )
                    , ( "hit", flash == Just { key = keyName, ok = True } )
                    , ( "miss", flash == Just { key = keyName, ok = False } )
                    , ( "bump", keyName == "f" || keyName == "j" )
                    ]
                ]
                [ Html.text label ]

        letter char =
            key "" (String.fromChar char) (String.fromChar char) (Layout.fingerFor layout char)

        spacer widthInKeys =
            Html.div [ A.class "spacer", A.style "width" ("calc(var(--key) * " ++ widthInKeys ++ ")") ] []

        row leading chars =
            Html.div [ A.class "key-row" ] (leading ++ List.map letter (String.toList chars))

        rows =
            Layout.rows layout
    in
    Html.div [ A.class "keyboard-area" ]
        [ Html.div [ A.class "keyboard", A.attribute "aria-hidden" "true" ]
            (case rows of
                [ top, home, bottom ] ->
                    [ row [ key "wide" "Escape" "esc" (Just Layout.LeftLittle) ] top
                    , row [ spacer "1.75" ] home
                    , row [ spacer "2.25" ] bottom
                    , Html.div [ A.class "key-row" ] [ spacer "1.5", key "space" " " texts.space (Just Layout.Thumb) ]
                    ]

                _ ->
                    []
            )
        , Html.p [ A.class "finger-hint" ] [ Html.text (fingerHint texts layout focus.target) ]
        ]


fingerHint : Texts -> Layout -> Maybe Char -> String
fingerHint texts layout target =
    case target of
        Just char ->
            let
                keyName =
                    if char == ' ' then
                        texts.space

                    else
                        String.fromChar char
            in
            texts.nextKey keyName (Layout.fingerFor layout char |> Maybe.map texts.finger |> Maybe.withDefault "")

        Nothing ->
            texts.keyboardLegend


{-| CSS classes f0 to f8 carry the finger colours.
-}
fingerClass : Finger -> String
fingerClass finger =
    Layout.allFingers
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, f ) -> f == finger)
        |> List.head
        |> Maybe.map (\( index, _ ) -> "f" ++ String.fromInt index)
        |> Maybe.withDefault "f8"
