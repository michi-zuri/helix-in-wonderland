module DrinkMePlaythroughTest exposing (suite)

{-| Play chapter 4: change the stars into words, sip by sip, with `e`, `c`,
`j k` and `.` (#74).
-}

import Chapter.Contract exposing (Context)
import Chapter.DrinkMe as DrinkMe
import Chapter.Story as Story
import Editor.Core as Core exposing (Mode(..))
import Editor.Session as Session
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Set
import Test exposing (Test, describe, test)
import Texts exposing (Texts)


contextFor : Language -> Context
contextFor language =
    { texts = Texts.forLanguage language, layout = UsQwerty }


context : Context
context =
    contextFor English


send : DrinkMe.Msg -> Run DrinkMe.Model -> Run DrinkMe.Model
send =
    sendIn context


sendIn : Context -> DrinkMe.Msg -> Run DrinkMe.Model -> Run DrinkMe.Model
sendIn ctx =
    Playthrough.send (DrinkMe.update ctx)


readStory : Run DrinkMe.Model -> Run DrinkMe.Model
readStory =
    Playthrough.sendAll (DrinkMe.update context) (List.repeat 8 (DrinkMe.KeyPressed "Enter"))


{-| Read the story, pick a difficulty ("1" easy, "2" challenge, "3" mad),
and close the dialog that tells how it works.
-}
startIn : Context -> String -> Run DrinkMe.Model
startIn ctx difficulty =
    Playthrough.start DrinkMe.init
        |> Playthrough.sendAll (DrinkMe.update ctx) (List.repeat 8 (DrinkMe.KeyPressed "Enter"))
        |> sendIn ctx (DrinkMe.KeyPressed difficulty)
        |> closeDialogs ctx


start : String -> Run DrinkMe.Model
start =
    startIn context


{-| A player pressing a key every 200 ms of game time, who reads every
dialog that comes up (Enter) before the next key.
-}
playIn : Context -> List String -> Run DrinkMe.Model -> Run DrinkMe.Model
playIn ctx keys run =
    List.foldl
        (\key current ->
            closeDialogs ctx current
                |> sendIn ctx (DrinkMe.KeyPressed key)
                |> sendIn ctx (DrinkMe.Frame 200)
        )
        run
        keys


{-| Read every dialog to its end: Enter shows the whole text, then closes it.
-}
closeDialogs : Context -> Run DrinkMe.Model -> Run DrinkMe.Model
closeDialogs ctx run =
    if Story.isTalking run.model.story then
        Playthrough.sendAll (DrinkMe.update ctx) [ DrinkMe.KeyPressed "Enter", DrinkMe.KeyPressed "Enter" ] run

    else
        run


play : List String -> Run DrinkMe.Model -> Run DrinkMe.Model
play =
    playIn context


pause : Float -> Run DrinkMe.Model -> Run DrinkMe.Model
pause milliseconds =
    Playthrough.sendAll (DrinkMe.update context) (List.repeat (round (milliseconds / 50)) (DrinkMe.Frame 50))


{-| Long enough for Alice to walk to the door.
-}
wait : Run DrinkMe.Model -> Run DrinkMe.Model
wait =
    pause 5000


letters : String -> List String
letters =
    String.toList >> List.map String.fromChar


{-| Our best route: `e c`, the word, `j k`; `e .` for a word straight after
the same word.
-}
route : List String -> List String
route words =
    List.map2
        (\previous word ->
            if previous == Just word then
                [ "e", "." ]

            else
                [ "e", "c" ] ++ letters word ++ [ "j", "k" ]
        )
        (Nothing :: List.map Just words)
        words
        |> List.concat


changeWord : String -> List String
changeWord word =
    "c" :: letters word ++ [ "j", "k" ]


wordsOf : Context -> String -> List String
wordsOf ctx difficulty =
    case difficulty of
        "1" ->
            ctx.texts.drinkMe.easyWords

        "2" ->
            ctx.texts.drinkMe.challengeWords

        _ ->
            ctx.texts.drinkMe.madWords


easy : List String
easy =
    context.texts.drinkMe.easyWords


lines : Run DrinkMe.Model -> List String
lines run =
    case run.model.work of
        Just work ->
            Session.exactLines (Core.toString (Session.buffer work.session))

        Nothing ->
            []


mode : Run DrinkMe.Model -> Maybe Mode
mode run =
    Maybe.map (.session >> Session.buffer >> Core.mode) run.model.work


keystrokes : Run DrinkMe.Model -> Int
keystrokes run =
    Maybe.map (.session >> Session.keystrokes) run.model.work |> Maybe.withDefault 0


hintsTold : Run DrinkMe.Model -> Int
hintsTold run =
    List.length run.model.hintsGiven


allLetters : Texts -> List Char
allLetters texts =
    List.concat [ texts.drinkMe.easyWords, texts.drinkMe.challengeWords, texts.drinkMe.madWords ]
        |> String.concat
        |> String.toList


averageLength : List String -> Float
averageLength words =
    toFloat (List.sum (List.map String.length words)) / toFloat (List.length words)


hasRepeat : List String -> Bool
hasRepeat words =
    List.map2 (==) words (List.drop 1 words) |> List.any identity


suite : Test
suite =
    describe "Chapter 4: Drink Me"
        [ describe "our best route, played through the chapter"
            (List.concatMap
                (\( language, name ) ->
                    let
                        ctx =
                            contextFor language
                    in
                    List.map
                        (\( difficulty, tarts ) ->
                            test (name ++ ", difficulty " ++ difficulty ++ ": finishes with all tarts, in exactly our best keystrokes") <|
                                \_ ->
                                    startIn ctx difficulty
                                        |> playIn ctx (route (wordsOf ctx difficulty))
                                        |> (\run -> ( keystrokes run, run |> wait |> readStory |> Playthrough.finishedWith ))
                                        |> Expect.equal ( DrinkMe.ourBest (wordsOf ctx difficulty), Just tarts )
                        )
                        [ ( "1", 1 ), ( "2", 2 ), ( "3", 3 ) ]
                )
                [ ( English, "English" ), ( German, "German" ) ]
            )
        , test "the lines start as stars, one line a word, after an empty first line" <|
            \_ ->
                start "1"
                    |> lines
                    |> Expect.equal ("" :: List.map (always "***") easy)
        , test "e then c changes the stars on the next line into the word" <|
            \_ ->
                start "1"
                    |> play ("e" :: changeWord "alas")
                    |> (\run -> ( List.take 3 (lines run), mode run ))
                    |> Expect.equal ( [ "", "alas", "***" ], Just Normal )
        , test ". repeats the last change on the next stars" <|
            \_ ->
                start "1"
                    |> play (route [ "alas", "she", "shall", "fade" ])
                    |> play [ "e", "." ]
                    |> lines
                    |> List.take 6
                    |> Expect.equal [ "", "alas", "she", "shall", "fade", "fade" ]
        , test "the closing story waits until Alice has walked to the door" <|
            \_ ->
                start "1"
                    |> play (route easy)
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , test "Alice gets smaller with the words; the closing story tells of the key on the table" <|
            \_ ->
                let
                    before =
                        start "1" |> wait

                    after =
                        before |> play (route easy) |> wait
                in
                ( DrinkMe.alicePixelSize after.model < DrinkMe.alicePixelSize before.model / 2
                , Story.isTalking after.model.story
                )
                    |> Expect.equal ( True, True )
        , describe "a change has to be quick"
            [ test "the time is 300 ms a letter of the word, plus 600 ms for j k" <|
                \_ -> DrinkMe.timeLimit "alas" |> Expect.equal 1800
            , test "too slow: the change is undone, back to the stars in Normal mode" <|
                \_ ->
                    start "1"
                        |> play [ "e", "c", "a", "l" ]
                        |> pause 2000
                        |> (\run -> ( List.take 2 (lines run), mode run, run.model.tooSlow ))
                        |> Expect.equal ( [ "", "***" ], Just Normal, 1 )
            , test "just in time is fine" <|
                \_ ->
                    -- 1800 ms for alas: a wait of 750 ms after c, then a l a s j
                    -- k with 200 ms between the keys: k comes after 1750 ms.
                    start "1"
                        |> play [ "e" ]
                        |> send (DrinkMe.KeyPressed "c")
                        |> pause 750
                        |> play (letters "alas" ++ [ "j", "k" ])
                        |> (\run -> ( List.take 2 (lines run), run.model.tooSlow ))
                        |> Expect.equal ( [ "", "alas" ], 0 )
            , test "the time doesn't run while a dialog is read" <|
                \_ ->
                    -- The first i shows a dialog (e c . are quicker); reading it takes a while.
                    start "1"
                        |> play [ "i" ]
                        |> pause 10000
                        |> (\run -> ( mode run, run.model.tooSlow ))
                        |> Expect.equal ( Just Insert, 0 )
            , test ". takes no time, so it is never too slow" <|
                \_ ->
                    start "1"
                        |> play (route [ "alas", "she", "shall", "fade" ] ++ [ "e", "." ])
                        |> pause 3000
                        |> .model
                        |> .tooSlow
                        |> Expect.equal 0
            ]
        , describe "a wrong word"
            [ test "escaped in time, it stays and counts as a wrong word" <|
                \_ ->
                    start "1"
                        |> play ("e" :: changeWord "alsa")
                        |> (\run -> ( List.take 2 (lines run), run.model.wrongWords ))
                        |> Expect.equal ( [ "", "alsa" ], 1 )
            , test "k, e and c change it again, even on the first word" <|
                \_ ->
                    start "1"
                        |> play ("e" :: changeWord "alsa")
                        |> play ([ "k", "e" ] ++ changeWord "alas")
                        |> lines
                        |> List.take 3
                        |> Expect.equal [ "", "alas", "***" ]
            , test "further down, e may need pressing twice after k" <|
                \_ ->
                    start "1"
                        |> play (route [ "alas" ] ++ ("e" :: changeWord "sh"))
                        |> play ([ "k", "e", "e" ] ++ changeWord "she")
                        |> lines
                        |> List.take 4
                        |> Expect.equal [ "", "alas", "she", "***" ]
            , test "mad: two wrong words cost a tart" <|
                \_ ->
                    let
                        words =
                            context.texts.drinkMe.madWords
                    in
                    start "3"
                        |> play ("e" :: changeWord "x" ++ [ "k", "e" ] ++ changeWord "y" ++ [ "k", "e" ])
                        |> play (List.drop 1 (route words))
                        |> wait
                        |> readStory
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 2)
            ]
        , describe "hints come once, when they help"
            [ test "h j k l and i work, but the first time j is used a dialog says e c . are quicker" <|
                \_ ->
                    start "1"
                        |> play [ "j" ]
                        |> (\run -> ( Story.isTalking run.model.story, List.member DrinkMe.OtherKeys run.model.hintsGiven ))
                        |> Expect.equal ( True, True )
            , test "the dialog comes only once" <|
                \_ ->
                    start "1"
                        |> play [ "j", "l", "h", "j" ]
                        |> hintsTold
                        |> Expect.equal 2
            , test "k, the way back to a wrong word, doesn't bring it" <|
                \_ ->
                    start "1"
                        |> play [ "e", "k" ]
                        |> .model
                        |> .hintsGiven
                        |> List.member DrinkMe.OtherKeys
                        |> Expect.equal False
            , test "the same word coming next brings the hint for ." <|
                \_ ->
                    start "1"
                        |> play (route [ "alas", "she", "shall", "fade" ])
                        |> .model
                        |> .hintsGiven
                        |> List.member DrinkMe.Repeat
                        |> Expect.equal True
            , test "too slow a second time: only a toast" <|
                \_ ->
                    start "1"
                        |> play [ "e", "c" ]
                        |> pause 2000
                        |> play [ "c" ]
                        |> pause 2000
                        |> Playthrough.toastCount
                        |> Expect.equal 2
            ]
        , describe "one word a line"
            [ test "Enter in INSERT adds no line" <|
                \_ ->
                    start "1"
                        |> play [ "e", "c", "Enter", "a" ]
                        |> lines
                        |> List.length
                        |> Expect.equal (1 + List.length easy)
            , test "Backspace at the start of a line doesn't join it to the line above" <|
                \_ ->
                    start "1"
                        |> play [ "e", "c", "Backspace" ]
                        |> lines
                        |> List.take 2
                        |> Expect.equal [ "", "" ]
            ]
        , describe "the word lists"
            (List.concatMap
                (\language ->
                    let
                        texts =
                            Texts.forLanguage language

                        name =
                            if language == English then
                                "English"

                            else
                                "German"
                    in
                    [ test (name ++ ": only the home row (no j, which starts j k), e, c, i and the full stop") <|
                        \_ ->
                            allLetters texts
                                |> List.filter (\char -> not (String.contains (String.fromChar char) "asdfghkl;öeic."))
                                |> Set.fromList
                                |> Set.toList
                                |> Expect.equal []
                    , test (name ++ ": every list has a word straight after the same word, for .") <|
                        \_ ->
                            [ texts.drinkMe.easyWords, texts.drinkMe.challengeWords, texts.drinkMe.madWords ]
                                |> List.map hasRepeat
                                |> Expect.equal [ True, True, True ]
                    , test (name ++ ": challenge words are longer than easy ones") <|
                        \_ ->
                            (averageLength texts.drinkMe.challengeWords - averageLength texts.drinkMe.easyWords)
                                |> Expect.atLeast 1
                    , test (name ++ ": mad words are at least ten letters long") <|
                        \_ ->
                            texts.drinkMe.madWords
                                |> List.filter (\word -> String.length word < 10)
                                |> Expect.equal []
                    ]
                )
                [ English, German ]
            )
        ]
