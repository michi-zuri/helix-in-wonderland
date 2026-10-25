module DrinkMePlaythroughTest exposing (suite)

{-| Play chapter 3: type the words, sip by sip.
-}

import Chapter.Contract exposing (Context)
import Chapter.DrinkMe as DrinkMe
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Test exposing (Test, describe, test)
import Texts


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
second.
-}
typeSteadily : String -> Run DrinkMe.Model -> Run DrinkMe.Model
typeSteadily text run =
    String.toList text
        |> List.foldl (\char -> send (DrinkMe.KeyPressed (String.fromChar char)) >> send (DrinkMe.Frame 200)) run


{-| Long enough for Alice to walk to the door.
-}
wait : Run DrinkMe.Model -> Run DrinkMe.Model
wait =
    Playthrough.sendAll (DrinkMe.update context) (List.repeat 100 (DrinkMe.Frame 50))


{-| All the words with a space between them, and none after the last.
-}
allWords : String
allWords =
    String.join " " (Texts.forLanguage English).drinkMe.words


start : Run DrinkMe.Model
start =
    Playthrough.start DrinkMe.init |> readStory


suite : Test
suite =
    describe "Chapter 3: Drink Me"
        [ test "typing every word without a mistake earns three tarts" <|
            \_ ->
                start
                    |> typeSteadily allWords
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 3)
        , test "the closing story waits until Alice has walked to the door" <|
            \_ ->
                start
                    |> typeSteadily allWords
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , test "a wrong key before every word costs two tarts" <|
            \_ ->
                start
                    |> typeSteadily (String.replace " " " x" allWords)
                    |> wait
                    |> readStory
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , test "the last letter of the last word ends the chapter, no space needed" <|
            \_ ->
                start
                    |> typeSteadily allWords
                    |> .model
                    |> (\model -> ( model.walk /= Nothing, model.waitingForSpace ))
                    |> Expect.equal ( True, False )
        , test "the space bar only counts once the word is complete" <|
            \_ ->
                start
                    |> typeSteadily "as k"
                    |> .model
                    |> (\model -> ( model.wordIndex, model.letterIndex, model.errors ))
                    |> Expect.equal ( 0, 3, 1 )
        , test "speed is measured on the game clock: five keys a second is about 60 words a minute" <|
            \_ ->
                start
                    |> typeSteadily allWords
                    |> .model
                    |> DrinkMe.wordsPerMinute
                    |> toFloat
                    |> Expect.within (Expect.Absolute 2) 61
        ]
