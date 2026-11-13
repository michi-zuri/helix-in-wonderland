module LittleDoorPlaythroughTest exposing (suite)

{-| Play chapter 3: open the little door's locks with the golden key, in all
three difficulties and both languages.
-}

import Chapter.Contract exposing (Context)
import Chapter.LittleDoor as LittleDoor
import Chapter.Story as Story
import Dict
import Editor.Core as Core exposing (Mode(..))
import Editor.Session as Session
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Texts exposing (Texts)


contextIn : Language -> Context
contextIn language =
    { texts = Texts.forLanguage language, layout = SwissQwertz, escapeKeys = Nothing }


english : Context
english =
    contextIn English


{-| Press keys like a player: a hint dialog that pops up (#93) is read
with Enter before the next key.
-}
pressIn : Context -> List String -> Run LittleDoor.Model -> Run LittleDoor.Model
pressIn context pressed run =
    List.foldl
        (\key current ->
            Playthrough.send (LittleDoor.update context)
                (LittleDoor.KeyPressed key)
                (if key == "Enter" then
                    current

                 else
                    readDialogs context 20 current
                )
        )
        run
        pressed


readDialogs : Context -> Int -> Run LittleDoor.Model -> Run LittleDoor.Model
readDialogs context fuel run =
    if fuel > 0 && Story.isTalking run.model.story && Story.isPlaying run.model.story == False && run.model.difficulty /= Nothing && not run.model.doorOpen then
        readDialogs context (fuel - 1) (Playthrough.send (LittleDoor.update context) (LittleDoor.KeyPressed "Enter") run)

    else
        run


{-| Read the story, then pick a difficulty: "1" easy, "2" challenge, "3" mad.
-}
startIn : Context -> String -> Run LittleDoor.Model
startIn context difficulty =
    Playthrough.start LittleDoor.init
        |> pressIn context (List.repeat 12 "Enter")
        |> pressIn context [ difficulty ]


start : String -> Run LittleDoor.Model
start =
    startIn english


{-| The closing story has two lines: Enter shows each in full, Enter goes on.
-}
readOutro : Context -> Run LittleDoor.Model -> Run LittleDoor.Model
readOutro context =
    pressIn context (List.repeat 4 "Enter")


wait : Float -> Run LittleDoor.Model -> Run LittleDoor.Model
wait milliseconds =
    Playthrough.sendAll (LittleDoor.update english) (List.repeat (round (milliseconds / 50)) (LittleDoor.Frame 50))


{-| A route as one string per step: every character is a key, `⎋` is Esc and
`⌫` is Backspace. "6l" is "6" then "l"; "ihad⎋" puts the key in, types
"had" and pulls the key out.
-}
keys : List String -> List String
keys steps =
    String.concat steps
        |> String.toList
        |> List.map
            (\char ->
                case char of
                    '⎋' ->
                        "Escape"

                    '⌫' ->
                        "Backspace"

                    _ ->
                        String.fromChar char
            )


{-| Our ways through every puzzle, with the keys taught by chapter 3 only:
`h j k l`, counts, `i`, `c` and Esc. Mad's are the "Our best" routes.
-}
route : Language -> String -> List String
route language difficulty =
    case ( language, difficulty ) of
        ( English, "1" ) ->
            [ "6lihad⎋", "jcglad⎋", "j2lihalf⎋", "j9lchall⎋" ]

        ( English, "2" ) ->
            [ "6lihad⎋", "23lcglad⎋", "j17hchalf⎋", "2lias⎋", "7lias⎋", "j24hishall⎋", "2lcask⎋", "j4liall⎋", "12lihad⎋", "4lcflash⎋" ]

        ( English, _ ) ->
            [ "6lihad⎋", "35lcglad⎋", "j6hihad⎋", "5lcask⎋", "j29hishall⎋", "11lcall⎋", "j3lchalf⎋", "2lias⎋", "j24hcalas⎋", "j32lias⎋", "6lihad⎋", "j18hia⎋", "2lchall⎋", "j15lihad⎋" ]

        ( German, "1" ) ->
            [ "6lisah⎋", "j5hclag⎋", "j15lials⎋", "j18hcsah⎋" ]

        ( German, "2" ) ->
            [ "6lisah⎋", "24lclag⎋", "j9hials⎋", "7lcdas⎋", "j30hilass⎋", "21lcja⎋", "j13hidass⎋", "5lcda⎋" ]

        ( German, _ ) ->
            [ "6lisah⎋", "j5hilag⎋", "29lcsah⎋", "j8hida⎋", "j6lials⎋", "17lcdas⎋", "j36hilass⎋", "21lcja⎋", "jhidas⎋", "j5hida⎋", "j11lcsah⎋" ]


nameOf : Language -> String
nameOf language =
    case language of
        English ->
            "English"

        German ->
            "German"


textOf : Run LittleDoor.Model -> String
textOf run =
    run.model.work |> Maybe.map (.session >> Session.buffer >> Core.toString) |> Maybe.withDefault ""


modeOf : Run LittleDoor.Model -> Maybe Mode
modeOf run =
    run.model.work |> Maybe.map (.session >> Session.buffer >> Core.mode)


keystrokesOf : Run LittleDoor.Model -> Int
keystrokesOf run =
    run.model.work |> Maybe.map (.session >> Session.keystrokes) |> Maybe.withDefault 0


stageHas : String -> Run LittleDoor.Model -> Expect.Expectation
stageHas wanted run =
    (LittleDoor.view english run.model).stage |> Query.fromHtml |> Query.has [ text wanted ]


hints : Texts -> List String
hints texts =
    [ texts.littleDoor.hintFind, texts.littleDoor.hintPin, texts.littleDoor.hintTurn Nothing, texts.littleDoor.hintTurn (Just ";;"), texts.littleDoor.hintBound ]


{-| A hint dialog is open (#93), it shows `wanted` once its letters have
appeared, and no other hint.
-}
onlyHint : String -> Run LittleDoor.Model -> Expect.Expectation
onlyHint wanted run =
    let
        shown =
            wait 3000 run
    in
    hints english.texts
        |> List.filter (\hint -> (LittleDoor.view english shown.model).stage |> Query.fromHtml |> Query.has [ text hint ] |> (==) Expect.pass)
        |> Expect.equal [ wanted ]


{-| Every line of the open dialog, read to its end with Enter.
-}
dialogLines : Context -> Run LittleDoor.Model -> List String
dialogLines context run =
    let
        go fuel current seen =
            if fuel <= 0 || not (Story.isTalking current.model.story) then
                List.reverse seen

            else
                let
                    shown =
                        Playthrough.sendAll (LittleDoor.update context) (List.repeat 60 (LittleDoor.Frame 50)) current

                    lines =
                        allLines context.texts
                            |> List.filter (\line -> (LittleDoor.view context shown.model).stage |> Query.fromHtml |> Query.has [ text line ] |> (==) Expect.pass)
                in
                go (fuel - 1) (Playthrough.sendAll (LittleDoor.update context) [ LittleDoor.KeyPressed "Enter" ] shown) (List.reverse lines ++ seen)
    in
    go 20 run [] |> unique


allLines : Texts -> List String
allLines texts =
    hints texts ++ [ texts.caterpillar.escTip, texts.escapeKeysWork ";;", texts.littleDoor.keySlipped ]


unique : List String -> List String
unique =
    List.foldl
        (\line found ->
            if List.member line found then
                found

            else
                found ++ [ line ]
        )
        []


{-| How often each character occurs.
-}
counts : String -> Dict.Dict Char Int
counts string =
    String.foldl (\char -> Dict.update char (\n -> Just (Maybe.withDefault 0 n + 1))) Dict.empty string


{-| The characters the answer has more of than the start: what the player
types.
-}
typed : String -> String -> List Char
typed startText answer =
    let
        before =
            counts startText
    in
    counts answer
        |> Dict.toList
        |> List.filter (\( char, n ) -> n > Maybe.withDefault 0 (Dict.get char before))
        |> List.map Tuple.first


puzzles : Texts -> List ( String, String, String )
puzzles texts =
    [ ( "easy", texts.littleDoor.easyStart, texts.littleDoor.easyAnswer )
    , ( "challenge", texts.littleDoor.challengeStart, texts.littleDoor.challengeAnswer )
    , ( "mad", texts.littleDoor.madStart, texts.littleDoor.madAnswer )
    ]


clean : String -> String
clean =
    String.words >> String.join " "


suite : Test
suite =
    describe "Chapter 3: The Little Door"
        [ describe "the difficulty sets the most tarts there are"
            (List.concatMap
                (\language ->
                    let
                        context =
                            contextIn language
                    in
                    List.map
                        (\( difficulty, tarts ) ->
                            test (nameOf language ++ " " ++ difficulty) <|
                                \_ ->
                                    startIn context difficulty
                                        |> pressIn context (keys (route language difficulty))
                                        |> readOutro context
                                        |> Playthrough.finishedWith
                                        |> Expect.equal (Just tarts)
                        )
                        [ ( "1", 1 ), ( "2", 2 ), ( "3", 3 ) ]
                )
                [ English, German ]
            )
        , describe "mad: our best way opens every lock in the keystrokes shown"
            (List.map
                (\language ->
                    let
                        context =
                            contextIn language
                    in
                    test (nameOf language) <|
                        \_ ->
                            startIn context "3"
                                |> pressIn context (keys (route language "3"))
                                |> keystrokesOf
                                |> Expect.equal context.texts.littleDoor.madBest
                )
                [ English, German ]
            )
        , describe "the puzzles"
            (List.concatMap
                (\language ->
                    let
                        texts =
                            Texts.forLanguage language
                    in
                    List.concatMap
                        (\( name, startText, answer ) ->
                            [ test (nameOf language ++ " " ++ name ++ ": everything typed is on the home row, in lowercase") <|
                                \_ ->
                                    typed startText answer
                                        |> List.filter (\char -> not (String.contains (String.fromChar char) "asdfghjklö"))
                                        |> Expect.equal []
                            , test (nameOf language ++ " " ++ name ++ ": every line has a lock") <|
                                \_ ->
                                    List.map2 (\a b -> clean a == clean b) (String.lines startText) (String.lines answer)
                                        |> List.filter identity
                                        |> Expect.equal []
                            ]
                        )
                        (puzzles texts)
                )
                [ English, German ]
            )
        , test "the door stays shut while the key is still in the last lock" <|
            \_ ->
                start "1"
                    |> pressIn english (keys [ "6l", "ihad⎋", "j", "iglad⎋", "j", "2l", "ihalf⎋", "j", "11l", "cl" ])
                    |> readOutro english
                    |> Playthrough.finishedWith
                    |> Expect.equal Nothing
        , describe "the key is Insert mode"
            [ test "i puts the key in: INSERT" <|
                \_ -> start "1" |> pressIn english [ "i" ] |> modeOf |> Expect.equal (Just Insert)
            , test "with the key in, h j k l type letters" <|
                \_ ->
                    start "1"
                        |> pressIn english [ "i", "j", "k" ]
                        |> textOf
                        |> String.startsWith "jkAlice"
                        |> Expect.equal True
            , test "Esc pulls it out: NORMAL" <|
                \_ -> start "1" |> pressIn english [ "i", "Escape" ] |> modeOf |> Expect.equal (Just Normal)
            , test "c on an emoji lets it tumble out and puts the key in (#107)" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "j", "8l", "c" ])
                        |> (\run -> ( modeOf run, String.contains "She was  to see" (textOf run) ))
                        |> Expect.equal ( Just Insert, True )
            , test "the player's own escape keys pull the key out too" <|
                \_ ->
                    let
                        withEscapeKeys =
                            { english | escapeKeys = Just ( ';', ';' ) }
                    in
                    startIn withEscapeKeys "1"
                        |> pressIn withEscapeKeys
                            (List.concatMap
                                (\key ->
                                    if key == "Escape" then
                                        [ ";", ";" ]

                                    else
                                        [ key ]
                                )
                                (keys (route English "1"))
                            )
                        |> readOutro withEscapeKeys
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
            ]
        , describe "keys that come later stay locked"
            (List.map
                (\key ->
                    test key <|
                        \_ ->
                            start "1"
                                |> pressIn english (String.split " " key)
                                |> (\run -> ( textOf run, modeOf run ))
                                |> Expect.equal ( english.texts.littleDoor.easyStart ++ "\n", Just Normal )
                )
                [ "w d", "x d", "a", "g l", "u", "e", "b" ]
            )
        , describe "mad: the key slips out if you wait"
            [ test "after more than two seconds with the key in, back to NORMAL; the first time a dialog says why" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "6", "l", "i", "h" ]
                        |> wait (LittleDoor.slipMilliseconds + 200)
                        |> (\later -> ( modeOf later, later.model.slips, dialogLines english later ))
                        |> Expect.equal ( Just Normal, 1, [ english.texts.littleDoor.keySlipped ] )
            , test "later slips only get a short note" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "6", "l", "i", "h" ]
                        |> wait (LittleDoor.slipMilliseconds + 200)
                        |> pressIn english [ "i", "a" ]
                        |> (\run -> ( run, Playthrough.toastCount run ))
                        |> (\( run, before ) ->
                                wait (LittleDoor.slipMilliseconds + 200) run
                                    |> (\later -> ( later.model.slips, Playthrough.toastCount later - before, Story.isTalking later.model.story ))
                           )
                        |> Expect.equal ( 2, 1, False )
            , test "reading a hint never makes the key slip: closing the dialog counts as a key" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "6", "l", "i" ]
                        -- The dialog about the key is open now; the player takes their time.
                        |> wait 8000
                        |> Playthrough.sendAll (LittleDoor.update english) (List.repeat 12 (LittleDoor.KeyPressed "Enter"))
                        |> wait (LittleDoor.slipMilliseconds - 200)
                        |> modeOf
                        |> Expect.equal (Just Insert)
            , test "a little under two seconds is fine" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "6", "l", "i", "h" ]
                        |> wait (LittleDoor.slipMilliseconds - 200)
                        |> modeOf
                        |> Expect.equal (Just Insert)
            , test "every key typed starts the two seconds again" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "6", "l", "i", "h" ]
                        |> wait 1500
                        |> pressIn english [ "a" ]
                        |> wait 1500
                        |> modeOf
                        |> Expect.equal (Just Insert)
            , test "on challenge the key never slips" <|
                \_ ->
                    start "2"
                        |> pressIn english [ "i" ]
                        |> wait 10000
                        |> modeOf
                        |> Expect.equal (Just Insert)
            , test "the results show how often it slipped" <|
                \_ ->
                    start "3"
                        |> pressIn english [ "i", "a", "Backspace" ]
                        |> wait (LittleDoor.slipMilliseconds + 200)
                        |> pressIn english (keys (route English "3"))
                        |> (\run -> run.model.slips)
                        |> Expect.equal 1
            ]
        , test "mad: many Backspaces cost tarts" <|
            \_ ->
                start "3"
                    |> pressIn english (keys [ "6l", "ihhhhhhhhhh⌫⌫⌫⌫⌫⌫⌫⌫⌫⌫had⎋" ])
                    |> pressIn english (keys (List.drop 1 (route English "3")))
                    |> readOutro english
                    |> Playthrough.finishedWith
                    |> Expect.equal (Just 1)
        , describe "each hint is told once, in a dialog, the first time it helps (#93)"
            [ test "first: find a lock" <|
                \_ -> start "1" |> onlyHint english.texts.littleDoor.hintFind
            , test "with the key in: type, then Esc" <|
                \_ -> start "1" |> pressIn english (keys [ "6l", "i" ]) |> onlyHint (english.texts.littleDoor.hintTurn Nothing)
            , test "typing h j k l with the key in, as if to walk: the key binds you" <|
                \_ -> start "1" |> pressIn english (keys [ "6l", "ij" ]) |> onlyHint english.texts.littleDoor.hintBound
            , test "on an emoji: c (#107)" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "6lihad⎋", "j" ])
                        |> onlyHint english.texts.littleDoor.hintPin
            , test "without escape keys, on US QWERTY: the key-in hint names Esc, and Ctrl+[ follows" <|
                \_ ->
                    let
                        qwerty =
                            { english | layout = UsQwerty }
                    in
                    startIn qwerty "1"
                        |> pressIn qwerty (keys [ "6l", "i" ])
                        |> dialogLines qwerty
                        |> Expect.equal [ english.texts.littleDoor.hintTurn Nothing, english.texts.caterpillar.escTip ]
            , test "on QWERTZ, where [ needs AltGr, no word about Ctrl+[ (#97)" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "6l", "i" ])
                        |> dialogLines english
                        |> Expect.equal [ english.texts.littleDoor.hintTurn Nothing ]
            , test "German, with escape keys: the sentence names them (#97)" <|
                \_ ->
                    (contextIn German).texts.littleDoor.hintTurn (Just ";;")
                        |> Expect.equal "Der Schlüssel steckt. Tippe die fehlenden Buchstaben, dann ziehe den Schlüssel wieder heraus, indem du Esc oder deine alternative Tastenkombination ;; drückst."
            , test "with escape keys, the key-in hint names them, in one line (#97)" <|
                \_ ->
                    let
                        withEscapeKeys =
                            { english | escapeKeys = Just ( ';', ';' ) }
                    in
                    startIn withEscapeKeys "1"
                        |> pressIn withEscapeKeys (keys [ "6l", "i" ])
                        |> dialogLines withEscapeKeys
                        |> Expect.equal [ english.texts.littleDoor.hintTurn (Just ";;") ]
            , test "only once: the second time the key goes in, no dialog" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "6l", "ihad⎋", "j", "i" ])
                        |> (\run -> Story.isTalking run.model.story)
                        |> Expect.equal False
            , test "no panel: the goal stands under the lines, in the editor" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "6l" ])
                        |> Expect.all
                            [ stageHas english.texts.littleDoor.goal
                            , stageHas "She was glad to see a garden."
                            ]
            ]
        , describe "the door opens only on exactly the goal (#101)"
            (let
                answer =
                    english.texts.littleDoor.challengeAnswer

                opens text =
                    Session.matchesExactly answer (Session.start text)

                finishes steps =
                    start "1"
                        |> pressIn english (keys steps)
                        |> readOutro english
                        |> Playthrough.finishedWith
             in
             [ test "the reported text: the four lines with spaces around them, and three lines too many" <|
                \_ ->
                    opens "Alice had the key, and she was glad. \n   The door was half as tall as she was.   \n\"I shall try the lock,\" said Alice. \nShe asked the door to open, and it did.      \nThe door was  as tall as she wds. \n\"I  try the lock,\" said Alice. \nShe ssked the door to open, and it dif. \n"
                        |> Expect.equal False
             , test "the four lines exactly, but one line too many" <|
                \_ -> opens (answer ++ "\nAlice\n") |> Expect.equal False
             , test "a space at the end of a line counts" <|
                \_ -> opens (String.replace "glad." "glad. " answer) |> Expect.equal False
             , test "a double space counts" <|
                \_ -> opens (String.replace "the key" "the  key" answer) |> Expect.equal False
             , test "exactly the goal opens it" <|
                \_ -> opens answer |> Expect.equal True
             , test "in play: a double space left in, the door stays shut" <|
                \_ ->
                    finishes [ "6l", "ihad ⎋", "j", "iglad⎋", "j", "2l", "ihalf⎋", "j", "11l", "cl⎋" ]
                        |> Expect.equal Nothing
             , test "in play: a new line typed with Enter and taken back with Backspace, the door opens (#103)" <|
                \_ ->
                    start "1"
                        |> pressIn english (keys [ "6l", "ihad" ] ++ [ "Enter", "Backspace", "Escape" ])
                        |> pressIn english (keys (List.drop 1 (route English "1")))
                        |> readOutro english
                        |> Playthrough.finishedWith
                        |> Expect.equal (Just 1)
             ]
            )
        , describe "one lock per line, gold while the line is exact (#103)"
            (let
                locksAfter steps =
                    start "1"
                        |> pressIn english steps
                        |> (\run -> LittleDoor.locks english.texts run.model)
             in
             [ test "four lines, four locks, none open yet" <|
                \_ -> locksAfter [] |> Expect.equal [ False, False, False, False ]
             , test "a line made right turns its lock gold" <|
                \_ -> locksAfter (keys [ "6l", "ihad⎋" ]) |> Expect.equal [ True, False, False, False ]
             , test "Enter splits a line: five lines, five locks, and the fifth never opens" <|
                \_ ->
                    locksAfter (keys [ "6l", "ihad⎋", "20l", "i" ] ++ [ "Enter" ])
                        |> Expect.equal [ False, False, False, False, False ]
             , test "many lines too many: still only one extra lock (#105)" <|
                \_ ->
                    locksAfter (keys [ "6l", "ihad⎋", "i" ] ++ List.repeat 6 "Enter")
                        |> Expect.equal [ False, False, False, False, False ]
             , test "Backspace joins the lines again: four locks, the first gold again" <|
                \_ ->
                    locksAfter (keys [ "6l", "ihad⎋", "20l", "i" ] ++ [ "Enter", "Backspace" ])
                        |> Expect.equal [ True, False, False, False ]
             , test "two lines joined: three locks" <|
                \_ ->
                    locksAfter (keys [ "j", "20h", "i" ] ++ [ "Backspace" ])
                        |> List.length
                        |> Expect.equal 3
             , test "joined and split again with Enter: four locks" <|
                \_ ->
                    locksAfter (keys [ "j", "20h", "i" ] ++ [ "Backspace", "Enter" ])
                        |> List.length
                        |> Expect.equal 4
             ]
            )
        , test "the text follows the language until a difficulty is picked" <|
            \_ ->
                Playthrough.start LittleDoor.init
                    |> pressIn english (List.repeat 12 "Enter")
                    |> pressIn (contextIn German) [ "1" ]
                    |> pressIn english [ "l" ]
                    |> textOf
                    |> Expect.equal ((contextIn German).texts.littleDoor.easyStart ++ "\n")
        ]
