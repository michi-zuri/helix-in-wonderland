module ProgressAndSettingsTest exposing (suite)

import Chapter.Id as ChapterId exposing (ChapterId(..))
import Expect
import Json.Decode as Decode
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Progress
import Settings
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "saved progress and settings"
        [ describe "which chapters are open"
            [ test "the first chapter is always open" <|
                \_ -> Progress.isOpen { unlockAll = False } RabbitHole Progress.empty |> Expect.equal True
            , test "a chapter opens once the one before has a tart" <|
                \_ ->
                    ( Progress.isOpen { unlockAll = False } HallOfDoors Progress.empty
                    , Progress.isOpen { unlockAll = False } HallOfDoors (Progress.record RabbitHole 1 Progress.empty)
                    )
                        |> Expect.equal ( False, True )
            , test "a chapter added in between doesn't lock away a chapter already played" <|
                \_ ->
                    Progress.empty
                        |> Progress.record DrinkMe 2
                        |> Progress.record PoolOfTears 3
                        |> (\progress -> ( Progress.isOpen { unlockAll = False } Curiouser progress, Progress.isOpen { unlockAll = False } PoolOfTears progress ))
                        |> Expect.equal ( True, True )
            , test "the new chapter's saved name is its own" <|
                \_ -> List.map ChapterId.toKey ChapterId.all |> Expect.equal [ "rabbit-hole", "hall-of-doors", "drink-me", "curiouser", "pool-of-tears", "caterpillar" ]
            , test "grown-ups can open everything" <|
                \_ -> Progress.isOpen { unlockAll = True } CaterpillarsAdvice Progress.empty |> Expect.equal True
            , test "only the best result for a chapter is kept" <|
                \_ ->
                    Progress.empty
                        |> Progress.record DrinkMe 3
                        |> Progress.record DrinkMe 1
                        |> Progress.tartsFor DrinkMe
                        |> Expect.equal 3
            ]
        , describe "settings"
            [ test "a Swiss browser gets German with a Swiss keyboard" <|
                \_ ->
                    Settings.guess "de-CH"
                        |> (\s -> ( s.language, s.layout ))
                        |> Expect.equal ( German, SwissQwertz )
            , test "a French browser gets an AZERTY keyboard" <|
                \_ -> (Settings.guess "fr-FR").layout |> Expect.equal FrenchAzerty
            , test "missing or unknown saved values fall back to the guess" <|
                \_ ->
                    Decode.decodeString (Settings.decoder (Settings.guess "de-CH")) """{ "layout": "dvorak", "soundOn": false }"""
                        |> Result.map (\s -> ( s.language, s.layout, s.soundOn ))
                        |> Expect.equal (Ok ( German, SwissQwertz, False ))
            , test "settings survive saving and loading" <|
                \_ ->
                    let
                        settings =
                            { language = English, layout = FrenchAzerty, soundOn = False, unlockAll = True, escapeKeys = Just ";;" }
                    in
                    Settings.encode settings
                        |> Decode.decodeValue (Settings.decoder (Settings.guess "de-CH"))
                        |> Expect.equal (Ok settings)
            ]
        ]
