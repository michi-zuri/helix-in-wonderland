module RabbitHolePlaythroughTest exposing (suite)

{-| Play chapter 1 with a fixed random seed, so every run sees the same jars.
-}

import Chapter.Contract exposing (Context)
import Chapter.RabbitHole as RabbitHole
import Chapter.Story as Story
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


{-| A player who reads the story and always presses the letter of the jar
to catch next. `mistakeEvery` presses a wrong key now and then (0 = never).
After the goal they practise until `practiceJars` more jars are caught,
then press Esc to land. Stops when the chapter reports its score, or after
`fuel` moves.
-}
play : Layout -> Int -> Int -> Int -> Run RabbitHole.Model -> Run RabbitHole.Model
play layout mistakeEvery practiceJars fuel run =
    let
        send =
            Playthrough.send (RabbitHole.update (contextFor layout))

        jarToCatch =
            run.model.jars |> List.sortBy .y |> List.head

        next =
            play layout mistakeEvery practiceJars (fuel - 1)
    in
    if fuel <= 0 || Playthrough.finishedWith run /= Nothing then
        run

    else if Story.isTalking run.model.story then
        next (send (RabbitHole.KeyPressed "Enter") run)

    else if run.model.practising && run.model.practiceCatches >= practiceJars then
        next (send (RabbitHole.KeyPressed "Escape") run)

    else
        case jarToCatch of
            Just jar ->
                let
                    withMistake =
                        if mistakeEvery > 0 && modBy mistakeEvery fuel == 0 then
                            send (RabbitHole.KeyPressed "z")

                        else
                            identity
                in
                next (run |> withMistake |> send (RabbitHole.KeyPressed (String.fromChar jar.letter)))

            Nothing ->
                next (send (RabbitHole.Frame 50) run)


{-| Play up to the goal and stop there, without landing.
-}
toTheGoal : Run RabbitHole.Model -> Run RabbitHole.Model
toTheGoal run =
    playUntil (\model -> model.practising) 5000 run


playUntil : (RabbitHole.Model -> Bool) -> Int -> Run RabbitHole.Model -> Run RabbitHole.Model
playUntil done fuel run =
    let
        send =
            Playthrough.send (RabbitHole.update (contextFor UsQwerty))
    in
    if fuel <= 0 || done run.model then
        run

    else if Story.isTalking run.model.story then
        playUntil done (fuel - 1) (send (RabbitHole.KeyPressed "Enter") run)

    else
        case run.model.jars |> List.sortBy .y |> List.head of
            Just jar ->
                playUntil done (fuel - 1) (send (RabbitHole.KeyPressed (String.fromChar jar.letter)) run)

            Nothing ->
                playUntil done (fuel - 1) (send (RabbitHole.Frame 50) run)


lettersSeen : Layout -> List Char
lettersSeen layout =
    let
        collect fuel run seen =
            if fuel <= 0 || Playthrough.finishedWith run /= Nothing then
                seen

            else
                let
                    send =
                        Playthrough.send (RabbitHole.update (contextFor layout))

                    next =
                        if Story.isTalking run.model.story then
                            send (RabbitHole.KeyPressed "Enter") run

                        else
                            case run.model.jars |> List.sortBy .y |> List.head of
                                Just jar ->
                                    send (RabbitHole.KeyPressed (String.fromChar jar.letter)) run

                                Nothing ->
                                    send (RabbitHole.Frame 50) run
                in
                if run.model.practising then
                    seen

                else
                    collect (fuel - 1) next (List.map .letter run.model.jars ++ seen)
    in
    collect 5000 start []


suite : Test
suite =
    describe "Chapter 1: Down the Rabbit Hole"
        [ test "catching every jar without a mistake earns three tarts" <|
            \_ ->
                start
                    |> play SwissQwertz 0 0 5000
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "reaching the goal opens a practice round instead of ending" <|
            \_ ->
                start
                    |> toTheGoal
                    |> (\run -> ( run.model.hits, run.model.practising, Playthrough.finishedWith run ))
                    |> Expect.equal ( 40, True, Nothing )
        , test "the jars rise faster and faster, also in practice" <|
            \_ ->
                let
                    speedAfter catches =
                        start
                            |> playUntil (\model -> model.hits >= catches && not (List.isEmpty model.jars)) 8000
                            |> .model
                            |> .jars
                            |> List.map .speed
                            |> List.minimum
                            |> Maybe.withDefault 0
                in
                Expect.all
                    [ \_ -> Expect.greaterThan (speedAfter 0 * 1.5) (speedAfter 30)
                    , \_ -> Expect.greaterThan (speedAfter 30) (speedAfter 70)
                    ]
                    ()
        , test "mistakes while practising don't cost tarts, and Esc lands" <|
            \_ ->
                start
                    |> toTheGoal
                    |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 20 (RabbitHole.KeyPressed "z"))
                    |> play UsQwerty 0 5 5000
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "all forty jars must be caught to reach the goal" <|
            \_ ->
                start
                    |> play SwissQwertz 0 0 5000
                    |> .model
                    |> .hits
                    |> Expect.equal 40
        , test "frequent wrong keys cost tarts" <|
            \_ ->
                start
                    |> play SwissQwertz 3 0 5000
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "a Swiss keyboard practises ö, never ;" <|
            \_ ->
                lettersSeen SwissQwertz
                    |> (\letters -> ( List.member 'ö' letters, List.member ';' letters ))
                    |> Expect.equal ( True, False )
        , test "a French keyboard practises q and m for the little fingers" <|
            \_ ->
                lettersSeen FrenchAzerty
                    |> (\letters -> ( List.member 'q' letters && List.member 'm' letters, List.member 'a' letters ))
                    |> Expect.equal ( True, False )
        , test "jars that float away are not counted as caught, and come back" <|
            \_ ->
                start
                    |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 8 (RabbitHole.KeyPressed "Enter"))
                    |> Playthrough.sendAll (RabbitHole.update (contextFor UsQwerty)) (List.repeat 600 (RabbitHole.Frame 50))
                    |> (\run -> ( run.model.hits, run.model.stage, List.isEmpty run.model.jars ))
                    |> Expect.equal ( 0, 0, False )
        ]
