module RabbitHolePlaythroughTest exposing (suite)

{-| Play the Rabbit Hole with a fixed random seed, so every run sees the same
jars: chapter 3 with both hands, and chapter 1 with the right hand only.

The player in these tests reads the story, picks a difficulty with its
number key, and presses the letter of every jar the moment it appears. That
is faster than any child, which makes the timings below the shortest
possible.

-}

import Chapter.Contract exposing (Context)
import Chapter.RabbitHole as RabbitHole
import Chapter.Story as Story
import Difficulty exposing (Difficulty(..))
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Random
import Test exposing (Test, describe, test)
import Texts


contextFor : Layout -> Context
contextFor layout =
    { texts = Texts.forLanguage German, layout = layout, escapeKeys = Nothing }


start : Run RabbitHole.Model
start =
    startWith RabbitHole.BothHands


startWith : RabbitHole.Hands -> Run RabbitHole.Model
startWith hands =
    Playthrough.start (RabbitHole.init hands (Random.initialSeed 42))


type alias Player =
    { layout : Layout
    , difficulty : Difficulty
    , mistakeEvery : Int
    , letGoAtDepth : Maybe Int
    }


player : Difficulty -> Player
player difficulty =
    { layout = UsQwerty, difficulty = difficulty, mistakeEvery = 0, letGoAtDepth = Nothing }


numberKey : Difficulty -> String
numberKey difficulty =
    case difficulty of
        Easy ->
            "1"

        Challenge ->
            "2"

        Mad ->
            "3"


{-| One move of the player: whatever the screen asks for next.
-}
step : Player -> Int -> Run RabbitHole.Model -> Run RabbitHole.Model
step who moveNumber run =
    let
        send =
            Playthrough.send (RabbitHole.update (contextFor who.layout))

        model =
            run.model
    in
    if Story.isTalking model.story then
        send (RabbitHole.KeyPressed "Enter") run

    else if model.difficulty == Nothing then
        send (RabbitHole.KeyPressed (numberKey who.difficulty)) run

    else if model.landing /= Nothing then
        send (RabbitHole.Frame 50) run

    else if Maybe.map (\depth -> model.depth >= depth) who.letGoAtDepth == Just True then
        send (RabbitHole.KeyPressed "Escape") run

    else
        case model.jars |> List.sortBy .y |> List.head of
            Just jar ->
                let
                    withMistake =
                        if who.mistakeEvery > 0 && modBy who.mistakeEvery moveNumber == 0 then
                            send (RabbitHole.KeyPressed "z")

                        else
                            identity
                in
                run |> withMistake |> send (RabbitHole.KeyPressed (String.fromChar jar.letter))

            Nothing ->
                send (RabbitHole.Frame 50) run


{-| Play until `done` says so, at most `fuel` moves.
-}
playUntil : (Run RabbitHole.Model -> Bool) -> Player -> Int -> Run RabbitHole.Model -> Run RabbitHole.Model
playUntil done who fuel run =
    if fuel <= 0 || done run then
        run

    else
        playUntil done who (fuel - 1) (step who fuel run)


finished : Run RabbitHole.Model -> Bool
finished run =
    Playthrough.finishedWith run /= Nothing


playToTheEnd : Player -> Run RabbitHole.Model
playToTheEnd who =
    playUntil finished who 20000 start


playToTheEndWith : RabbitHole.Hands -> Player -> Run RabbitHole.Model
playToTheEndWith hands who =
    playUntil finished who 20000 (startWith hands)


{-| Game seconds between the last basic jar (the 40th) and touching down.
-}
secondsBeyondBasicJars : Difficulty -> Float
secondsBeyondBasicJars difficulty =
    let
        atBasicJars =
            playUntil (\run -> run.model.hits >= 40) (player difficulty) 20000 start

        atGround =
            playUntil (\run -> run.model.landing /= Nothing) (player difficulty) 20000 atBasicJars
    in
    (atGround.model.clock - atBasicJars.model.clock) / 1000


lettersSeenInStages : Layout -> List Char
lettersSeenInStages =
    lettersSeenWith RabbitHole.BothHands


{-| Every letter on a jar during the six stages and well beyond (mad goes on
after the stages, with all the chapter's keys).
-}
lettersSeenWith : RabbitHole.Hands -> Layout -> List Char
lettersSeenWith hands layout =
    let
        collect fuel run seen =
            if fuel <= 0 || run.model.hits >= 60 then
                seen

            else
                collect (fuel - 1) (step { layout = layout, difficulty = Mad, mistakeEvery = 0, letGoAtDepth = Nothing } fuel run) (List.map .letter run.model.jars ++ seen)
    in
    collect 20000 (startWith hands) []


chosen : Difficulty -> Run RabbitHole.Model
chosen difficulty =
    playUntil (\run -> run.model.difficulty /= Nothing) (player difficulty) 100 start


distinct : List Char -> List Char
distinct letters =
    List.foldl
        (\letter found ->
            if List.member letter found then
                found

            else
                found ++ [ letter ]
        )
        []
        letters
        |> List.sort


rightHandTests : Test
rightHandTests =
    describe "Chapter 1: Down the Rabbit Hole, right hand only"
        [ describe "the same difficulties and tarts as with both hands"
            (List.map
                (\( difficulty, tarts ) ->
                    test (numberKey difficulty) <|
                        \_ ->
                            playToTheEndWith RabbitHole.RightHand (player difficulty)
                                |> Playthrough.finishedWith
                                |> Expect.equal (Just tarts)
                )
                [ ( Easy, 1 ), ( Challenge, 2 ), ( Mad, 3 ) ]
            )
        , describe "only the right hand's keys: its home row and h"
            (List.map
                (\( layout, keys ) ->
                    test (Keyboard.Layout.toKey layout) <|
                        \_ ->
                            lettersSeenWith RabbitHole.RightHand layout
                                |> distinct
                                |> Expect.equal (List.sort keys)
                )
                [ ( UsQwerty, [ 'h', 'j', 'k', 'l', ';' ] )
                , ( SwissQwertz, [ 'h', 'j', 'k', 'l', 'ö' ] )
                , ( GermanQwertz, [ 'h', 'j', 'k', 'l', 'ö' ] )
                , ( FrenchAzerty, [ 'h', 'j', 'k', 'l', 'm' ] )
                ]
            )
        , test "the first jars are all J, for the index finger" <|
            \_ ->
                startWith RabbitHole.RightHand
                    |> playUntil (\run -> run.model.hits >= 6) (player Easy) 2000
                    |> (\run -> ( run.model.hits, run.model.stage ))
                    |> Expect.equal ( 6, 1 )
        , test "all its keys on a Swiss keyboard: J K L Ö, and H" <|
            \_ ->
                RabbitHole.keysOf RabbitHole.RightHand SwissQwertz
                    |> List.sort
                    |> Expect.equal (List.sort [ 'h', 'j', 'k', 'l', 'ö' ])
        ]


suite : Test
suite =
    describe "The Rabbit Hole"
        [ rightHandTests
        , bothHandsTests
        ]


bothHandsTests : Test
bothHandsTests =
    describe "Chapter 3: Down the Rabbit Hole, both hands"
        [ describe "difficulties"
            [ test "easy: the 40 basic jars reach the ground, worth one tart" <|
                \_ ->
                    playToTheEnd (player Easy)
                        |> (\run -> ( run.model.hits, Playthrough.finishedWith run ))
                        |> Expect.equal ( 40, Just 1 )
            , test "challenge: a few seconds beyond the basic jars, worth two tarts" <|
                \_ ->
                    Expect.all
                        [ \_ -> playToTheEnd (player Challenge) |> Playthrough.finishedWith |> Expect.equal (Just 2)
                        , \_ -> secondsBeyondBasicJars Challenge |> Expect.all [ Expect.atLeast 3, Expect.atMost 10 ]
                        ]
                        ()
            , test "mad: nearly thirty seconds beyond the basic jars, worth three tarts" <|
                \_ ->
                    Expect.all
                        [ \_ -> playToTheEnd (player Mad) |> Playthrough.finishedWith |> Expect.equal (Just 3)
                        , \_ -> secondsBeyondBasicJars Mad |> Expect.all [ Expect.atLeast 25, Expect.atMost 32 ]
                        ]
                        ()
            , test "the deeper Alice is, the faster the jars, and mad is the fastest" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.lessThan (RabbitHole.jarSpeed Easy 20) (RabbitHole.jarSpeed Easy 0)
                        , \_ -> Expect.lessThan (RabbitHole.jarSpeed Easy 40) (RabbitHole.jarSpeed Easy 20)
                        , \_ ->
                            Expect.lessThan
                                (RabbitHole.jarSpeed Challenge (RabbitHole.groundDepth Challenge))
                                (RabbitHole.jarSpeed Easy (RabbitHole.groundDepth Easy))
                        , \_ ->
                            Expect.lessThan
                                (RabbitHole.jarSpeed Mad (RabbitHole.groundDepth Mad))
                                (RabbitHole.jarSpeed Challenge (RabbitHole.groundDepth Challenge))
                        ]
                        ()
            , test "Enter right after the story doesn't pick a difficulty by accident" <|
                \_ ->
                    start
                        |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 12 (RabbitHole.KeyPressed "Enter"))
                        |> (\run -> run.model.difficulty)
                        |> Expect.equal Nothing
            , test "after a moment, j and Enter pick the highlighted difficulty" <|
                \_ ->
                    start
                        |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 12 (RabbitHole.KeyPressed "Enter"))
                        |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty))
                            (List.repeat 12 (RabbitHole.Frame 50) ++ [ RabbitHole.KeyPressed "j", RabbitHole.KeyPressed "Enter" ])
                        |> (\run -> run.model.difficulty)
                        |> Expect.equal (Just Challenge)
            ]
        , describe "mistakes"
            [ test "a wrong key lifts Alice three jars back up" <|
                \_ ->
                    chosen Easy
                        |> playUntil (\run -> run.model.depth >= 5) (player Easy) 1000
                        |> Playthrough.send (RabbitHole.update (contextFor UsQwerty)) (RabbitHole.KeyPressed "z")
                        |> (\run -> run.model.depth)
                        |> Expect.equal 2
            , test "a jar that floats away counts as a mistake" <|
                \_ ->
                    chosen Easy
                        |> playUntil (\run -> run.model.depth >= 4) (player Easy) 1000
                        |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 300 (RabbitHole.Frame 50))
                        |> (\run -> ( run.model.depth < 4, run.model.errors > 0 ))
                        |> Expect.equal ( True, True )
            , test "with mistakes, easy needs extra jars but still lands with one tart" <|
                \_ ->
                    playToTheEnd { layout = UsQwerty, difficulty = Easy, mistakeEvery = 7, letGoAtDepth = Nothing }
                        |> (\run -> ( run.model.hits > 40, Playthrough.finishedWith run ))
                        |> Expect.equal ( True, Just 1 )
            ]
        , describe "letting go with Esc"
            [ test "too early earns no tart" <|
                \_ ->
                    playToTheEnd { layout = UsQwerty, difficulty = Mad, mistakeEvery = 0, letGoAtDepth = Just 10 }
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 0)
            , test "in mad, past the easy ground, earns one tart" <|
                \_ ->
                    playToTheEnd { layout = UsQwerty, difficulty = Mad, mistakeEvery = 0, letGoAtDepth = Just (RabbitHole.groundDepth Easy) }
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
            , test "in mad, past the challenge ground, earns two tarts" <|
                \_ ->
                    playToTheEnd { layout = UsQwerty, difficulty = Mad, mistakeEvery = 0, letGoAtDepth = Just (RabbitHole.groundDepth Challenge) }
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 2)
            , test "the score comes only after the landing and the closing story" <|
                \_ ->
                    chosen Easy
                        |> playUntil (\run -> run.model.landing /= Nothing) (player Easy) 20000
                        |> (\run -> ( Playthrough.finishedWith run, Story.isTalking run.model.story ))
                        |> Expect.equal ( Nothing, False )
            ]
        , describe "keyboard layouts"
            [ test "a Swiss keyboard practises ö, never ;" <|
                \_ ->
                    lettersSeenInStages SwissQwertz
                        |> (\letters -> ( List.member 'ö' letters, List.member ';' letters ))
                        |> Expect.equal ( True, False )
            , test "a French keyboard practises q and m for the little fingers" <|
                \_ ->
                    lettersSeenInStages FrenchAzerty
                        |> (\letters -> ( List.member 'q' letters && List.member 'm' letters, List.member 'a' letters ))
                        |> Expect.equal ( True, False )
            ]
        ]
