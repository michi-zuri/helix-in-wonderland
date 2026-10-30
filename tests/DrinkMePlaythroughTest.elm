module DrinkMePlaythroughTest exposing (suite)

{-| Play chapter 3: type the words from memory, sip by sip.
-}

import Chapter.Contract exposing (Context)
import Chapter.DrinkMe as DrinkMe
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes)
import Texts exposing (Texts)


context : Context
context =
    { texts = Texts.forLanguage English, layout = UsQwerty }


send : DrinkMe.Msg -> Run DrinkMe.Model -> Run DrinkMe.Model
send =
    Playthrough.send (DrinkMe.update context)


readStory : Run DrinkMe.Model -> Run DrinkMe.Model
readStory =
    Playthrough.sendAll (DrinkMe.update context) (List.repeat 8 (DrinkMe.KeyPressed "Enter"))


{-| Every key is followed by 200 ms of game time: a steady five keys per
second, quick enough that no word starts over.
-}
typeSteadily : String -> Run DrinkMe.Model -> Run DrinkMe.Model
typeSteadily text run =
    String.toList text
        |> List.foldl (\char -> send (DrinkMe.KeyPressed (String.fromChar char)) >> send (DrinkMe.Frame 200)) run


pause : Float -> Run DrinkMe.Model -> Run DrinkMe.Model
pause milliseconds =
    Playthrough.sendAll (DrinkMe.update context) (List.repeat (round (milliseconds / 50)) (DrinkMe.Frame 50))


{-| Long enough for Alice to walk to the door.
-}
wait : Run DrinkMe.Model -> Run DrinkMe.Model
wait =
    pause 5000


{-| All the words with a space between them, and none after the last.
-}
allWords : List String -> String
allWords words =
    String.join " " words


easy : List String
easy =
    context.texts.drinkMe.easyWords


mad : List String
mad =
    context.texts.drinkMe.madWords


{-| Read the story, then pick a difficulty: "1" easy, "2" challenge, "3" mad.
-}
start : String -> Run DrinkMe.Model
start difficulty =
    Playthrough.start DrinkMe.init |> readStory |> send (DrinkMe.KeyPressed difficulty)


{-| How many letters of the current word, not yet typed, the panel shows.
-}
showsLetters : Int -> Run DrinkMe.Model -> Expect.Expectation
showsLetters count run =
    (DrinkMe.view context run.model).panel
        |> Query.fromHtml
        |> Query.find [ classes [ "word", "now" ] ]
        |> Query.findAll [ class "untyped" ]
        |> Query.count (Expect.equal count)


letters : Texts -> List Char
letters texts =
    List.concat [ texts.drinkMe.easyWords, texts.drinkMe.challengeWords, texts.drinkMe.madWords ]
        |> String.concat
        |> String.toList


averageLength : List String -> Float
averageLength words =
    toFloat (List.sum (List.map String.length words)) / toFloat (List.length words)


suite : Test
suite =
    describe "Chapter 3: Drink Me"
        [ describe "the difficulty sets the most tarts there are"
            [ test "easy: one tart" <|
                \_ ->
                    start "1"
                        |> typeSteadily (allWords easy)
                        |> wait
                        |> readStory
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
            , test "challenge: two tarts" <|
                \_ ->
                    start "2"
                        |> typeSteadily (allWords context.texts.drinkMe.challengeWords)
                        |> wait
                        |> readStory
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 2)
            , test "mad: three tarts" <|
                \_ ->
                    start "3"
                        |> typeSteadily (allWords mad)
                        |> wait
                        |> readStory
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 3)
            ]
        , test "the closing story waits until Alice has walked to the door" <|
            \_ ->
                start "1"
                    |> typeSteadily (allWords easy)
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , test "mad: three wrong keys before every word cost two tarts" <|
            \_ ->
                start "3"
                    |> typeSteadily ("xxx" ++ String.replace " " " xxx" (allWords mad))
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "the last letter of the last word ends the chapter, no space needed" <|
            \_ ->
                start "1"
                    |> typeSteadily (allWords easy)
                    |> .model
                    |> (\model -> ( model.walk /= Nothing, model.waitingForSpace ))
                    |> Expect.equal ( True, False )
        , test "the space bar only counts once the word is complete" <|
            \_ ->
                start "1"
                    |> Playthrough.sendAll (DrinkMe.update context) (List.map DrinkMe.KeyPressed [ "a", "l", " " ])
                    |> .model
                    |> (\model -> ( model.wordIndex, model.letterIndex, model.errors ))
                    |> Expect.equal ( 0, 2, 1 )
        , describe "words are typed from memory"
            [ test "the word shows until its first key" <|
                \_ -> start "1" |> showsLetters 4
            , test "then it hides" <|
                \_ -> start "1" |> typeSteadily "a" |> showsLetters 0
            , test "a pause longer than the limit in the middle of a word starts it over, shown again" <|
                \_ ->
                    start "1"
                        |> typeSteadily "al"
                        |> pause (DrinkMe.pauseLimit + 100)
                        |> Expect.all
                            [ \run -> Expect.equal ( 0, 1 ) ( run.model.letterIndex, run.model.startedOver )
                            , showsLetters 4
                            ]
            , test "a pause just under the limit is fine" <|
                \_ ->
                    start "1"
                        |> send (DrinkMe.KeyPressed "a")
                        |> pause (DrinkMe.pauseLimit - 50)
                        |> send (DrinkMe.KeyPressed "l")
                        |> .model
                        |> (\model -> ( model.letterIndex, model.startedOver ))
                        |> Expect.equal ( 2, 0 )
            , test "a wrong key doesn't stop the clock" <|
                \_ ->
                    start "1"
                        |> send (DrinkMe.KeyPressed "a")
                        |> pause 200
                        |> send (DrinkMe.KeyPressed "x")
                        |> pause 200
                        |> .model
                        |> .startedOver
                        |> Expect.equal 1
            , test "between words, and before the space, there is no hurry" <|
                \_ ->
                    start "1"
                        |> typeSteadily "alas"
                        |> pause 3000
                        |> typeSteadily " "
                        |> pause 3000
                        |> typeSteadily "she"
                        |> .model
                        |> (\model -> ( model.wordIndex, model.startedOver ))
                        |> Expect.equal ( 1, 0 )
            ]
        , test "speed is measured on the game clock: five keys a second is about 60 words a minute" <|
            \_ ->
                start "1"
                    |> typeSteadily (allWords easy)
                    |> .model
                    |> DrinkMe.wordsPerMinute
                    |> toFloat
                    |> Expect.within (Expect.Absolute 2) 61
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
                    [ test (name ++ ": only the home row, e and i") <|
                        \_ ->
                            letters texts
                                |> List.filter (\char -> not (String.contains (String.fromChar char) "asdfghjklei"))
                                |> Set.fromList
                                |> Set.toList
                                |> Expect.equal []
                    , test (name ++ ": challenge words are longer than easy ones") <|
                        \_ ->
                            (averageLength texts.drinkMe.challengeWords - averageLength texts.drinkMe.easyWords)
                                |> Expect.atLeast 1.5
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
