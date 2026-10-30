module Settings exposing (Settings, decoder, encode, guess)

{-| The player's choices, saved in the browser between visits.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Keyboard.Layout as Layout exposing (Layout(..))
import Language exposing (Language(..))


type alias Settings =
    { language : Language
    , layout : Layout
    , soundOn : Bool
    , unlockAll : Bool
    , escapeKeys : Maybe String
    }


{-| First visit: guess from the browser's language, e.g. "de-CH" gives
German with a Swiss keyboard.
-}
guess : String -> Settings
guess browserLanguage =
    let
        code =
            String.toLower browserLanguage

        layout =
            if String.endsWith "-ch" code then
                SwissQwertz

            else if String.startsWith "de" code then
                GermanQwertz

            else if String.startsWith "fr" code then
                FrenchAzerty

            else
                UsQwerty
    in
    { language =
        if String.startsWith "de" code then
            German

        else
            English
    , layout = layout
    , soundOn = True
    , unlockAll = False
    , escapeKeys = Nothing
    }


{-| Reads saved settings; anything missing or unknown falls back to `default`.
-}
decoder : Settings -> Decoder Settings
decoder default =
    Decode.map5 Settings
        (optional "language" (Decode.map Language.fromCode Decode.string) default.language)
        (optional "layout" (Decode.map Layout.fromKey Decode.string) default.layout)
        (optional "soundOn" (Decode.map Just Decode.bool) default.soundOn)
        (optional "unlockAll" (Decode.map Just Decode.bool) default.unlockAll)
        (optional "escapeKeys" (Decode.map (Just << Just) Decode.string) default.escapeKeys)


optional : String -> Decoder (Maybe a) -> a -> Decoder a
optional field valueDecoder fallback =
    Decode.oneOf
        [ Decode.field field valueDecoder |> Decode.map (Maybe.withDefault fallback)
        , Decode.succeed fallback
        ]


encode : Settings -> Encode.Value
encode settings =
    Encode.object
        [ ( "language", Encode.string (Language.toCode settings.language) )
        , ( "layout", Encode.string (Layout.toKey settings.layout) )
        , ( "soundOn", Encode.bool settings.soundOn )
        , ( "unlockAll", Encode.bool settings.unlockAll )
        , ( "escapeKeys", Maybe.map Encode.string settings.escapeKeys |> Maybe.withDefault Encode.null )
        ]
