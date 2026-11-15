module RabbitHolePlaythroughTest exposing (suite)

{-| Play chapter 1 with a fixed random seed, so every run sees the same jars.

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
    { texts = Texts.forLanguage German, layout = layout }


start : Run RabbitHole.Model
start =
    Playthrough.start (RabbitHole.init (Random.initialSeed 42))


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


{-| Game seconds between the last basic jar and touching down.
-}
secondsBeyondBasicJars : Difficulty -> Float
secondsBeyondBasicJars difficulty =
    let
        atBasicJars =
            playUntil (\run -> run.model.hits >= RabbitHole.basicJars) (player difficulty) 20000 start

        atGround =
            playUntil (\run -> run.model.landing /= Nothing) (player difficulty) 20000 atBasicJars
    in
    (atGround.model.clock - atBasicJars.model.clock) / 1000


{-| The letters on the jars of each stage, stage by stage, for a perfect
player on mad (who goes on after the stages).
-}
lettersByStage : Layout -> List (List Char)
lettersByStage layout =
    let
        who =
            { layout = layout, difficulty = Mad, mistakeEvery = 0, letGoAtDepth = Nothing }

        collect fuel run seen =
            if fuel <= 0 || run.model.landing /= Nothing then
                seen

            else
                let
                    stage =
                        run.model.stage

                    letters =
                        List.map .letter run.model.jars
                in
                collect (fuel - 1)
                    (step who fuel run)
                    (List.indexedMap
                        (\index found ->
                            if index == stage then
                                distinct (found ++ letters)

                            else
                                found
                        )
                        seen
                    )
    in
    collect 20000 start (List.repeat (RabbitHole.lastStage + 1) [])


{-| Every jar's letter as it appears, stage by stage, for a perfect player
on mad (who never misses, so each stage gets exactly its bag).
-}
spawnedByStage : List (List Char)
spawnedByStage =
    let
        who =
            player Mad

        collect fuel run nextId seen =
            if fuel <= 0 || run.model.landing /= Nothing then
                List.map List.reverse seen

            else
                let
                    new =
                        List.filter (\jar -> jar.id >= nextId) run.model.jars

                    added =
                        List.indexedMap
                            (\index found ->
                                if index == run.model.stage then
                                    List.foldl (\jar acc -> jar.letter :: acc) found new

                                else
                                    found
                            )
                            seen
                in
                collect (fuel - 1) (step who fuel run) run.model.nextJarId added
    in
    collect 20000 start 0 (List.repeat (RabbitHole.lastStage + 1) [])


{-| How often each letter comes, sorted by letter.
-}
counts : List Char -> List ( Char, Int )
counts letters =
    distinct letters |> List.map (\letter -> ( letter, List.length (List.filter ((==) letter) letters) ))


lettersSeenInStages : Layout -> List Char
lettersSeenInStages layout =
    List.concat (lettersByStage layout)


{-| A player who catches every jar until the last stage, reads its
instruction, then stops.
-}
stuckInTheLastStage : Run RabbitHole.Model
stuckInTheLastStage =
    chosen Easy
        |> playUntil (\run -> run.model.stage >= RabbitHole.lastStage && not (Story.isTalking run.model.story)) (player Easy) 20000


{-| The stages a player goes through until the last one, in order.
-}
stagesUntilTheLast : Player -> Run RabbitHole.Model -> List Int
stagesUntilTheLast who run =
    let
        go fuel current seen =
            if fuel <= 0 || current.model.stage >= RabbitHole.lastStage then
                List.reverse (current.model.stage :: seen)

            else if Just current.model.stage == List.head seen then
                go (fuel - 1) (step who fuel current) seen

            else
                go (fuel - 1) (step who fuel current) (current.model.stage :: seen)
    in
    go 20000 run []


idle : Float -> Run RabbitHole.Model -> Run RabbitHole.Model
idle milliseconds =
    Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat (round (milliseconds / 50)) (RabbitHole.Frame 50))


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


{-| The keys of each stage before the last, on a Swiss keyboard (#124).
-}
expectedStages : List (List Char)
expectedStages =
    [ [ 'f' ]
    , [ 'd' ]
    , [ 'd', 'f' ]
    , [ 's' ]
    , [ 'a' ]
    , [ 'a', 's' ]
    , [ 'd', 's' ]
    , [ 'a', 'd', 'f', 's' ]
    , [ 'j' ]
    , [ 'k' ]
    , [ 'j', 'k' ]
    , [ 'l' ]
    , [ 'ö' ]
    , [ 'l', 'ö' ]
    , [ 'k', 'l' ]
    , [ 'j', 'k', 'l', 'ö' ]
    , [ 'g', 'h' ]
    , [ 'f', 'g', 'h', 'j' ]
    , [ 'a', 'd', 'k', 'l', 's', 'ö' ]
    ]


suite : Test
suite =
    describe "Chapter 1: Down the Rabbit Hole"
        [ fallTests
        , readingTests
        , mainTests
        ]


fallTests : Test
fallTests =
    describe "one long fall"
        [ test "small steps (#124): each finger, pairs, the whole hand; the right hand the same; G H, the index fingers, the others" <|
            \_ ->
                -- Every letter seen belongs to the stage's keys.
                lettersByStage SwissQwertz
                    |> List.take RabbitHole.lastStage
                    |> List.map2 (\keys seen -> not (List.isEmpty seen) && List.all (\letter -> List.member letter keys) seen) expectedStages
                    |> Expect.equal (List.repeat RabbitHole.lastStage True)
        , test "a shuffle bag (#126): every stage brings each of its keys equally often, all of them" <|
            \_ ->
                spawnedByStage
                    |> List.take RabbitHole.lastStage
                    |> List.map counts
                    |> Expect.equal
                        [ [ ( 'f', 3 ) ]
                        , [ ( 'd', 3 ) ]
                        , [ ( 'd', 3 ), ( 'f', 3 ) ]
                        , [ ( 's', 3 ) ]
                        , [ ( 'a', 3 ) ]
                        , [ ( 'a', 3 ), ( 's', 3 ) ]
                        , [ ( 'd', 3 ), ( 's', 3 ) ]
                        , [ ( 'a', 3 ), ( 'd', 3 ), ( 'f', 3 ), ( 's', 3 ) ]
                        , [ ( 'j', 3 ) ]
                        , [ ( 'k', 3 ) ]
                        , [ ( 'j', 3 ), ( 'k', 3 ) ]
                        , [ ( 'l', 3 ) ]
                        , [ ( ';', 3 ) ]
                        , [ ( ';', 3 ), ( 'l', 3 ) ]
                        , [ ( 'k', 3 ), ( 'l', 3 ) ]
                        , [ ( ';', 3 ), ( 'j', 3 ), ( 'k', 3 ), ( 'l', 3 ) ]
                        , [ ( 'g', 3 ), ( 'h', 3 ) ]
                        , [ ( 'f', 3 ), ( 'g', 3 ), ( 'h', 3 ), ( 'j', 3 ) ]
                        , [ ( ';', 2 ), ( 'a', 2 ), ( 'd', 2 ), ( 'k', 2 ), ( 'l', 2 ), ( 's', 2 ) ]
                        ]
        , test "the last stage: bags of 30, each home-row key three times, freshly shuffled each time" <|
            \_ ->
                spawnedByStage
                    |> List.drop RabbitHole.lastStage
                    |> List.concat
                    |> (\letters -> ( counts (List.take 30 letters), counts (List.take 30 (List.drop 30 letters)), List.take 30 letters /= List.take 30 (List.drop 30 letters) ))
                    |> Expect.equal
                        ( List.map (\letter -> ( letter, 3 )) (List.sort (String.toList "asdfghjkl;"))
                        , List.map (\letter -> ( letter, 3 )) (List.sort (String.toList "asdfghjkl;"))
                        , True
                        )
        , test "a stage of one or two keys shows them all" <|
            \_ ->
                lettersByStage SwissQwertz
                    |> List.take RabbitHole.lastStage
                    |> List.map2 Tuple.pair expectedStages
                    |> List.filter (\( keys, _ ) -> List.length keys <= 2)
                    |> List.all (\( keys, seen ) -> keys == seen)
                    |> Expect.equal True
        , test "last, the whole home row: only home-row keys, from both hands" <|
            \_ ->
                lettersByStage SwissQwertz
                    |> List.drop RabbitHole.lastStage
                    |> List.concat
                    |> (\letters ->
                            ( List.all (\letter -> String.contains (String.fromChar letter) "asdfghjklö") letters
                            , List.any (\letter -> String.contains (String.fromChar letter) "asdf") letters
                                && List.any (\letter -> String.contains (String.fromChar letter) "jklö") letters
                            )
                       )
                    |> Expect.equal ( True, True )
        , describe "no stage brings more than five new keys"
            (List.map
                (\layout ->
                    test (Keyboard.Layout.toKey layout) <|
                        \_ ->
                            lettersByStage layout
                                |> List.foldl
                                    (\letters ( known, most ) ->
                                        ( distinct (known ++ letters)
                                        , max most (List.length (List.filter (\letter -> not (List.member letter known)) letters))
                                        )
                                    )
                                    ( [], 0 )
                                |> Tuple.second
                                |> Expect.atMost 5
                )
                Keyboard.Layout.all
            )
        , test "the stages before the whole home row have 114 jars; the grounds are at 120, 140 and 200" <|
            \_ ->
                [ RabbitHole.basicJars, RabbitHole.groundDepth Easy, RabbitHole.groundDepth Challenge, RabbitHole.groundDepth Mad ]
                    |> Expect.equal [ 114, 120, 140, 200 ]
        , describe "stuck in the last stage"
            [ test "80 seconds: the stuck time is 80 s" <|
                \_ -> RabbitHole.stuckMilliseconds |> Expect.equal 80000
            , test "just under it: still the last stage" <|
                \_ ->
                    stuckInTheLastStage
                        |> idle (RabbitHole.stuckMilliseconds - 1000)
                        |> (\run -> run.model.stage >= RabbitHole.lastStage)
                        |> Expect.equal True
            , test "after it: back to the first pair of fingers, with a word about it" <|
                \_ ->
                    stuckInTheLastStage
                        |> idle (RabbitHole.stuckMilliseconds + 1000)
                        |> (\run -> ( run.model.stage, Story.isTalking run.model.story ))
                        |> Expect.equal ( 2, True )
            , test "going round skips only the single fingers" <|
                \_ ->
                    stuckInTheLastStage
                        |> idle (RabbitHole.stuckMilliseconds + 1000)
                        |> stagesUntilTheLast (player Easy)
                        |> Expect.equal [ 2, 5, 6, 7, 10, 13, 14, 15, 16, 17, 18, 19 ]
            , test "only on easy: on challenge and mad the fall just goes on (#83)" <|
                \_ ->
                    [ Challenge, Mad ]
                        |> List.map
                            (\difficulty ->
                                chosen difficulty
                                    |> playUntil (\run -> run.model.stage >= RabbitHole.lastStage && not (Story.isTalking run.model.story)) (player difficulty) 20000
                                    |> idle (RabbitHole.stuckMilliseconds + 1000)
                                    |> (\run -> ( run.model.stage, Story.isTalking run.model.story ))
                            )
                        |> Expect.equal [ ( RabbitHole.lastStage, False ), ( RabbitHole.lastStage, False ) ]
            , test "a player who keeps going reaches the ground after going round" <|
                \_ ->
                    stuckInTheLastStage
                        |> idle (RabbitHole.stuckMilliseconds + 1000)
                        |> playUntil finished (player Easy) 20000
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
            ]
        ]


{-| Instructions come in dialogs that pause the fall (#81), so a player
can read them at their own pace.
-}
readingTests : Test
readingTests =
    describe "instructions are read at the player's pace"
        [ test "after picking a difficulty: how to catch, how to let go, the first stage; no jar rises meanwhile" <|
            \_ ->
                chosen Easy
                    |> idle 10000
                    |> (\run -> ( Story.isTalking run.model.story, run.model.jars, run.model.depth ))
                    |> Expect.equal ( True, [], 0 )
        , test "once the dialog is read, jars rise" <|
            \_ ->
                chosen Easy
                    |> playUntil (\run -> not (Story.isTalking run.model.story)) (player Easy) 100
                    |> idle 2000
                    |> (\run -> List.isEmpty run.model.jars)
                    |> Expect.equal False
        , test "every new stage opens with its instruction, and the fall waits for it" <|
            \_ ->
                chosen Easy
                    |> playUntil (\run -> run.model.stage == 1) (player Easy) 20000
                    |> idle 10000
                    |> (\run -> ( Story.isTalking run.model.story, run.model.jars ))
                    |> Expect.equal ( True, [] )
        , test "while a dialog shows, everything stands still: the walls, Alice, the clock (#124)" <|
            \_ ->
                let
                    reading =
                        chosen Easy
                            |> playUntil (\run -> run.model.stage == 1) (player Easy) 20000

                    later =
                        idle 5000 reading

                    still run =
                        ( run.model.scroll, run.model.aliceY, run.model.clock )
                in
                ( Story.isTalking later.model.story, still later )
                    |> Expect.equal ( True, still reading )
        , test "the last stage (the whole home row) never ends: the fall goes on with the same keys, no dialog (#83)" <|
            \_ ->
                chosen Mad
                    |> playUntil (\run -> run.model.hits >= RabbitHole.basicJars + 10) (player Mad) 20000
                    |> (\run -> ( Story.isTalking run.model.story, (RabbitHole.view (contextFor UsQwerty) run.model).status.pendingKeys ))
                    |> Expect.equal ( False, "A S D F G H J K L ;" )
        , test "no toasts any more: everything is said in dialogs" <|
            \_ ->
                playToTheEnd (player Easy)
                    |> Playthrough.toastCount
                    |> Expect.equal 0
        , test "the status line shows the stage's keys and the depth" <|
            \_ ->
                chosen Easy
                    |> playUntil (\run -> not (Story.isTalking run.model.story)) (player Easy) 100
                    |> (\run -> RabbitHole.view (contextFor UsQwerty) run.model)
                    |> (\view -> ( view.status.pendingKeys, view.status.position ))
                    |> Expect.equal ( "F", "0/" ++ String.fromInt (RabbitHole.groundDepth Easy) )
        ]


mainTests : Test
mainTests =
    describe "falling"
        [ describe "difficulties"
            [ test "easy: 120 jars without a mistake reach the ground, worth one tart" <|
                \_ ->
                    playToTheEnd (player Easy)
                        |> (\run -> ( run.model.hits, Playthrough.finishedWith run ))
                        |> Expect.equal ( RabbitHole.groundDepth Easy, Just 1 )
            , test "challenge: 140 jars, worth two tarts" <|
                \_ ->
                    playToTheEnd (player Challenge)
                        |> (\run -> ( run.model.hits, Playthrough.finishedWith run ))
                        |> Expect.equal ( 140, Just 2 )
            , test "mad: 200 jars, worth three tarts" <|
                \_ ->
                    playToTheEnd (player Mad)
                        |> (\run -> ( run.model.hits, Playthrough.finishedWith run ))
                        |> Expect.equal ( 200, Just 3 )
            , test "beyond the basic jars, challenge goes on about 20 seconds, mad nearly a minute" <|
                \_ ->
                    Expect.all
                        [ \_ -> secondsBeyondBasicJars Challenge |> Expect.all [ Expect.atLeast 15, Expect.atMost 25 ]
                        , \_ -> secondsBeyondBasicJars Mad |> Expect.all [ Expect.atLeast 50, Expect.atMost 62 ]
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
                        |> (\run -> ( run.model.hits > RabbitHole.groundDepth Easy, Playthrough.finishedWith run ))
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
