module CuriouserPlaythroughTest exposing (suite)

{-| Play chapter 4: squash the currants with Helix's `g` jumps.
-}

import Chapter.Contract exposing (Context)
import Chapter.Curiouser as Curiouser
import Chapter.Curiouser.Table as Table exposing (Trick(..))
import Difficulty exposing (Difficulty(..))
import Editor.Core as Core
import Editor.Session as Session exposing (Session)
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Random
import Test exposing (Test, describe, test)
import Texts


context : Context
context =
    { texts = Texts.forLanguage English, layout = UsQwerty }


press : List String -> Run Curiouser.Model -> Run Curiouser.Model
press keys =
    Playthrough.sendAll (Curiouser.update context) (List.map Curiouser.KeyPressed keys)


wait : Float -> Run Curiouser.Model -> Run Curiouser.Model
wait milliseconds =
    Playthrough.sendAll (Curiouser.update context) (List.repeat (round (milliseconds / 50)) (Curiouser.Frame 50))


{-| Read the story, then pick "1" easy, "2" challenge or "3" mad.
-}
start : String -> Run Curiouser.Model
start difficulty =
    Playthrough.start (Curiouser.init (Random.initialSeed 4))
        |> press (List.repeat 12 "Enter")
        |> press [ difficulty ]


readStory : Run Curiouser.Model -> Run Curiouser.Model
readStory =
    press (List.repeat 12 "Enter")


{-| The `gw` keys that land on a position: g, w and its label.
-}
gwTo : Int -> Run Curiouser.Model -> List String
gwTo position run =
    let
        labelled =
            press [ "g", "w" ] run
    in
    Session.jumpLabels labelled.model.session
        |> List.filter (\label -> label.position == position)
        |> List.head
        |> Maybe.map (\label -> "g" :: "w" :: List.map String.fromChar (String.toList label.label))
        |> Maybe.withDefault []


{-| A player who catches every currant with `gw` as soon as it appears.
-}
catchAll : Int -> Run Curiouser.Model -> Run Curiouser.Model
catchAll stepsLeft run =
    if stepsLeft <= 0 || Playthrough.finishedWith run /= Nothing || not (List.isEmpty run.model.letters) && run.model.nextLetter >= List.length run.model.letters && List.isEmpty run.model.currants then
        run

    else
        case run.model.currants of
            currant :: _ ->
                catchAll (stepsLeft - 1) (press (gwTo currant.position run) run)

            [] ->
                catchAll (stepsLeft - 1) (wait 100 run)


{-| From `cursor`, do `keys` take the cursor to where `trick` puts a
currant?
-}
trickReaches : Trick -> Table.Spot -> List String -> Bool
trickReaches trick cursor keys =
    let
        spot =
            Random.step (Table.place trick cursor) (Random.initialSeed 1) |> Tuple.first

        session =
            Session.fromBuffer (Core.withWrap Table.wrapWidth (Core.fromString Table.text))

        moveTo =
            sessionPress (List.concat (List.repeat cursor.line [ "g", "j" ]))
                >> sessionPress (List.repeat cursor.column "l")

        landed =
            session |> moveTo |> sessionPress keys
    in
    Core.head (Session.buffer landed) == Table.positionOf spot


sessionPress : List String -> Session -> Session
sessionPress keys session =
    List.foldl
        (\key current ->
            case Session.press (always True) key current of
                Session.Ran ran ->
                    ran.session

                Session.Waiting next ->
                    next

                Session.Locked next ->
                    next

                Session.Ignored next ->
                    next
        )
        session
        keys


suite : Test
suite =
    describe "Chapter 4: Curiouser and Curiouser"
        [ describe "each currant is placed where its jump is the quick way"
            [ test "at the far end of the line: gl" <|
                \_ -> trickReaches LineEnds { line = 0, column = 0 } [ "g", "l" ] |> Expect.equal True
            , test "or its start: gh" <|
                \_ -> trickReaches LineEnds { line = 6, column = 26 } [ "g", "h" ] |> Expect.equal True
            , test "on an indented line's first crumb: gs" <|
                \_ -> trickReaches FirstNonBlank { line = 1, column = 16 } [ "g", "s" ] |> Expect.equal True
            , test "on the real line after a long wrapped one: gj" <|
                \_ -> trickReaches RealLines { line = 2, column = 6 } [ "g", "j" ] |> Expect.equal True
            , test "from a short line, across the wrapped line above: gk gk" <|
                \_ -> trickReaches RealLines { line = 3, column = 6 } [ "g", "k", "g", "k" ] |> Expect.equal True
            ]
        , describe "every difficulty can be finished, for its tarts"
            (List.map
                (\( key, difficulty ) ->
                    test (Difficulty.name context.texts difficulty) <|
                        \_ ->
                            start key
                                |> catchAll 3000
                                |> wait 200
                                |> readStory
                                |> (\run -> ( Playthrough.finishedWith run, List.length run.model.escaped ))
                                |> Expect.equal ( Just (Difficulty.tarts difficulty), 0 )
                )
                [ ( "1", Easy ), ( "2", Challenge ), ( "3", Mad ) ]
            )
        , test "the currants' letters come in the order of the hidden sentence" <|
            \_ ->
                start "1"
                    |> catchAll 3000
                    |> .model
                    |> (\model -> List.filterMap (\index -> List.drop index model.letters |> List.head) model.squashed)
                    |> String.fromList
                    |> Expect.equal "EATMEANDGROWTALL"
        , test "a currant left alone rolls away after its time" <|
            \_ ->
                start "2"
                    |> wait (Curiouser.lifetime Challenge + 2000)
                    |> .model
                    |> .escaped
                    |> Expect.equal [ 0 ]
        , test "if every currant rolls away, the chapter still ends, with one tart" <|
            \_ ->
                start "3"
                    |> wait 400000
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "squashing really changes the table, so g. goes back there" <|
            \_ ->
                let
                    caught =
                        start "1" |> wait 1000 |> (\run -> press (Maybe.withDefault [] (Maybe.map (\c -> gwTo c.position run) (List.head run.model.currants))) run)

                    squashedAt =
                        caught.model.lastSquash
                in
                caught
                    |> press [ "g", "h", "j", "g", "." ]
                    |> .model
                    |> .session
                    |> Session.buffer
                    |> Core.head
                    |> Just
                    |> Expect.equal squashedAt
        , test "a cheeky currant runs back to the last squashed spot when the cursor comes near" <|
            \_ ->
                let
                    -- Catch currants until one that runs back appears.
                    findRunner steps run =
                        case List.filter .runsBack run.model.currants of
                            runner :: _ ->
                                Just ( runner, run )

                            [] ->
                                if steps <= 0 then
                                    Nothing

                                else
                                    case run.model.currants of
                                        currant :: _ ->
                                            findRunner (steps - 1) (press (gwTo currant.position run) run)

                                        [] ->
                                            findRunner (steps - 1) (wait 100 run)
                in
                case findRunner 2000 (start "1") of
                    Just ( runner, run ) ->
                        let
                            -- Land on a crumb next to the runner.
                            beside =
                                if Table.spotOf runner.position |> .column |> (\c -> c >= 3) then
                                    runner.position - 3

                                else
                                    runner.position + 3

                            near =
                                press (gwTo beside run) run
                        in
                        near.model.currants
                            |> List.map .position
                            |> Expect.equal (List.filterMap identity [ run.model.lastSquash ])

                    Nothing ->
                        Expect.fail "no cheeky currant came"
        , test "selecting and editing keys wait for later chapters" <|
            \_ ->
                start "1"
                    |> press [ "w", "x", "d", "i" ]
                    |> .model
                    |> .session
                    |> Session.buffer
                    |> Core.toString
                    |> Expect.equal Table.text
        , test "the table wraps: the long third line takes three screen rows" <|
            \_ ->
                String.lines Table.text
                    |> List.drop 2
                    |> List.head
                    |> Maybe.map (\row -> (String.length row + Table.wrapWidth - 1) // Table.wrapWidth)
                    |> Expect.equal (Just 3)
        ]
