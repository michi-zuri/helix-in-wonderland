module PoolPlaythroughTest exposing (suite)

{-| Play chapter 5 with real Helix keystrokes, in all three difficulties.
-}

import Chapter.Contract exposing (Context)
import Chapter.Pool as Pool
import Editor.Core as Core
import Editor.Session as Session
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Random
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Texts exposing (Texts)


contextIn : Language -> Context
contextIn language =
    { texts = Texts.forLanguage language, layout = UsQwerty }


english : Context
english =
    contextIn English


press : Context -> List String -> Run Pool.Model -> Run Pool.Model
press context pressed =
    Playthrough.sendAll (Pool.update context) (List.map Pool.KeyPressed pressed)


{-| Read the story, then pick a difficulty: "1" easy, "2" challenge, "3" mad.
-}
start : Context -> String -> Run Pool.Model
start context difficulty =
    Playthrough.start (Pool.init (Random.initialSeed 4))
        |> press context (List.repeat 12 "Enter")
        |> press context [ difficulty ]


readStory : Context -> Run Pool.Model -> Run Pool.Model
readStory context =
    press context (List.repeat 12 "Enter")


wait : Float -> Run Pool.Model -> Run Pool.Model
wait milliseconds =
    Playthrough.sendAll (Pool.update english) (List.repeat (round (milliseconds / 100)) (Pool.Frame 100))


{-| "12l" is typed as "1", "2", "l".
-}
keys : String -> List String
keys route =
    String.words route |> List.concatMap (String.toList >> List.map String.fromChar)


textOf : Run Pool.Model -> String
textOf run =
    run.model.work
        |> Maybe.map (.session >> Session.buffer >> Core.toString)
        |> Maybe.withDefault ""


keystrokesOf : Run Pool.Model -> Int
keystrokesOf run =
    run.model.work |> Maybe.map (.session >> Session.keystrokes) |> Maybe.withDefault 0


panelHas : String -> Run Pool.Model -> Expect.Expectation
panelHas wanted run =
    (Pool.view english run.model).panel |> Query.fromHtml |> Query.has [ text wanted ]


panelHasNot : String -> Run Pool.Model -> Expect.Expectation
panelHasNot unwanted run =
    (Pool.view english run.model).panel |> Query.fromHtml |> Query.hasNot [ text unwanted ]


{-| Our own best ways to clean the easy and mad poems; their keystrokes are
the "Our best" number on the results card.
-}
ourBest : List { language : Language, difficulty : String, route : String, best : Texts -> Int, tarts : Int }
ourBest =
    [ { language = English, difficulty = "1", route = "5w d j 8h w d j 2x d 4w d j 4w d", best = .pool >> .easyBest, tarts = 1 }
    , { language = English, difficulty = "3", route = "5w d j w d j 2x d 4w d j 4h w d", best = .pool >> .madBest, tarts = 3 }
    , { language = German, difficulty = "1", route = "5w d j 6h w d j 2x d 4w d j 4w d", best = .pool >> .easyBest, tarts = 1 }
    , { language = German, difficulty = "3", route = "5w d j 17h w d j 2x d 4w d j 6h w d", best = .pool >> .madBest, tarts = 3 }
    ]


{-| A patient player: press `w`, and `d` whenever the selection holds only
words that don't belong. Back to the top after each pass.
-}
cleanPatiently : Int -> Run Pool.Model -> Run Pool.Model
cleanPatiently stepsLeft run =
    case run.model.work of
        Just work ->
            let
                buffer =
                    Session.buffer work.session

                deleted =
                    Core.apply Core.Delete buffer

                wordsOf =
                    Core.toString >> String.words >> List.length

                safeToDelete =
                    Session.matches work.puzzle.cleanPoem (Session.start (Core.toString deleted))
                        || (wordsOf deleted < wordsOf buffer && fits work.puzzle.cleanPoem (Core.toString deleted))

                atEnd =
                    Core.apply Core.SelectNextWordStart buffer == buffer
            in
            if stepsLeft <= 0 || Session.matches work.puzzle.cleanPoem work.session then
                run

            else if safeToDelete then
                cleanPatiently (stepsLeft - 1) (press english [ "d" ] run)

            else if atEnd then
                cleanPatiently (stepsLeft - 1) (press english [ "2", "0", "k", "4", "0", "h" ] run)

            else
                cleanPatiently (stepsLeft - 1) (press english [ "w" ] run)

        Nothing ->
            run


fits : String -> String -> Bool
fits clean text =
    let
        wordsIn =
            String.words >> List.map (String.filter Char.isAlphaNum) >> List.filter (not << String.isEmpty)

        inside wanted words =
            case ( wanted, words ) of
                ( [], _ ) ->
                    True

                ( _, [] ) ->
                    False

                ( first :: restWanted, word :: restWords ) ->
                    if first == word then
                        inside restWanted restWords

                    else
                        inside wanted restWords
    in
    inside (wordsIn clean) (wordsIn text)


suite : Test
suite =
    describe "Chapter 5: The Pool of Tears"
        [ describe "our best ways clean the poems in the number of keystrokes shown"
            (List.map
                (\way ->
                    let
                        context =
                            contextIn way.language
                    in
                    test
                        ((if way.language == English then
                            "English"

                          else
                            "German"
                         )
                            ++ " "
                            ++ way.difficulty
                            ++ ": "
                            ++ way.route
                        )
                    <|
                        \_ ->
                            start context way.difficulty
                                |> press context (keys way.route)
                                |> (\run -> ( keystrokesOf run, readStory context run |> Playthrough.finishedWith ))
                                |> Expect.equal ( way.best context.texts, Just way.tarts )
                )
                ourBest
            )
        , test "the hidden sentence: easy's odd words read SWIM TO THE SHORE" <|
            \_ ->
                english.texts.pool.easyPoem
                    |> String.words
                    |> List.filter (\word -> String.toUpper word == word && String.any Char.isAlpha word && not (String.contains "," word))
                    |> Expect.equal [ "SWIM", "TO", "THE", "SHORE" ]
        , describe "deletions"
            [ test "deleting a poem word is a mistake; u brings it back" <|
                \_ ->
                    start english "3"
                        |> press english [ "w", "d" ]
                        |> Expect.all
                            [ \run -> Expect.equal 1 run.model.wrongDeletions
                            , panelHas english.texts.pool.hintUndo
                            , \run -> press english [ "u" ] run |> textOf |> Expect.equal (english.texts.pool.madPoem ++ "\n")
                            ]
            , test "one mistake on mad costs a tart" <|
                \_ ->
                    start english "3"
                        -- The undo brings back the selection too; 9h returns to the line start.
                        |> press english (keys "w d u 9h")
                        |> press english (keys "5w d j w d j 2x d 4w d j 4h w d")
                        |> readStory english
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 2)
            , test "on mad, the right 'golden' is a mistake to delete, the extra one is not" <|
                \_ ->
                    start english "3"
                        -- Line 6: "On every opening golden scale!"; the third word is the poem's own "golden".
                        |> press english (keys "5j 4w d")
                        |> .model
                        |> .wrongDeletions
                        |> Expect.equal 1
            ]
        , describe "keys"
            [ test "e and b wait for a later chapter" <|
                \_ ->
                    start english "1"
                        |> press english [ "e", "b" ]
                        |> textOf
                        |> Expect.equal (english.texts.pool.easyPoem ++ "\n")
            , test "counts work: 2x selects both lines of tears" <|
                \_ ->
                    start english "1"
                        |> press english (keys "2j 2x d")
                        |> textOf
                        |> String.contains "tears"
                        |> Expect.equal False
            ]
        , describe "hints come one at a time, when they help"
            [ test "first: find a word that doesn't belong" <|
                \_ -> start english "1" |> panelHas english.texts.pool.hintFind
            , test "with a word selected: d deletes it" <|
                \_ -> start english "1" |> press english [ "w" ] |> panelHas english.texts.pool.hintDelete
            , test "easy, on the lines of tears: 2x" <|
                \_ -> start english "1" |> press english (keys "2j") |> panelHas (english.texts.pool.hintLines 2)
            , test "mad never says which lines are odd" <|
                \_ -> start english "3" |> press english (keys "2j") |> panelHasNot (english.texts.pool.hintLines 2)
            , test "after w three times in a row: try a count" <|
                \_ -> start english "1" |> press english [ "w", "w", "w" ] |> panelHas english.texts.pool.hintCount
            , test "after walking left a few steps: try gh" <|
                \_ -> start english "1" |> press english (keys "8l h h h") |> panelHas english.texts.pool.hintLineStart
            , test "no gh tip once gh was used" <|
                \_ -> start english "1" |> press english (keys "8l gh 3l h h h") |> panelHasNot english.texts.pool.hintLineStart
            , test "no count tip once a count was used" <|
                \_ -> start english "1" |> press english [ "2", "w", "w", "w", "w" ] |> panelHasNot english.texts.pool.hintCount
            ]
        , describe "challenge: the rising tide"
            [ test "a new word splashes in every few seconds" <|
                \_ ->
                    start english "2"
                        |> wait 6100
                        |> textOf
                        |> String.words
                        |> List.member "A"
                        |> Expect.equal True
            , test "it stops after all of the tide's words" <|
                \_ ->
                    start english "2"
                        |> wait 120000
                        |> .model
                        |> .tide
                        |> .splashed
                        |> Expect.equal (List.length english.texts.pool.tideWords)
            , test "u undoes the player's change, never a splash" <|
                \_ ->
                    start english "2"
                        |> press english [ "x", "d" ]
                        |> wait 6100
                        |> press english [ "u" ]
                        |> textOf
                        |> String.words
                        |> List.member "A"
                        |> Expect.equal True
            , test "a clean poem doesn't end the chapter while the tide still has words" <|
                \_ ->
                    start english "2"
                        |> cleanPatiently 200
                        |> readStory english
                        |> Playthrough.finishedWith
                        |> Expect.equal Nothing
            , test "a patient player who keeps cleaning earns challenge's two tarts" <|
                \_ ->
                    List.range 1 8
                        |> List.foldl (\_ run -> run |> cleanPatiently 300 |> wait 6100) (start english "2")
                        |> cleanPatiently 300
                        |> readStory english
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 2)
            ]
        , test "the poem follows the language until a difficulty is picked" <|
            \_ ->
                Playthrough.start (Pool.init (Random.initialSeed 4))
                    |> press english (List.repeat 12 "Enter")
                    |> press (contextIn German) [ "1" ]
                    |> press english [ "l" ]
                    |> textOf
                    |> Expect.equal ((contextIn German).texts.pool.easyPoem ++ "\n")
        ]
