module Texts exposing
    ( Texts, ChapterInfo, ResultTexts, EndTexts
    , RabbitHoleTexts, HallTexts, DrinkMeTexts, PoolTexts, CaterpillarTexts
    , forLanguage
    )

{-| Every word the player reads, per language.

`Texts` is a record, so each language must fill in every field: a missing
German line is a compile error, not a blank on screen. German follows Swiss
spelling (ss instead of ß).

Story lines come from Lewis Carroll's 1865 book (public domain) or are newly
written for the game; the German versions are our own translations.

@docs Texts, ChapterInfo, ResultTexts, EndTexts
@docs RabbitHoleTexts, HallTexts, DrinkMeTexts, PoolTexts, CaterpillarTexts
@docs forLanguage

-}

import Chapter.Id exposing (ChapterId(..))
import Keyboard.Layout exposing (Finger(..))
import Language exposing (Language(..))
import Speaker exposing (Speaker(..))


type alias Texts =
    { logo : { before : String, small : String, after : String }
    , tagline : String
    , menuHint : String
    , menu : String
    , soundOn : String
    , soundOff : String
    , continueHint : String
    , footer : String
    , drawPictures : String
    , suggestChange : String
    , keyboardLegend : String
    , space : String
    , nextKey : String -> String -> String
    , finger : Finger -> String
    , speaker : Speaker -> String
    , layoutDetected : String -> String
    , notYet : String
    , locked : String
    , finishEarlierChapter : String
    , startHere : String
    , unlockAll : String
    , allUnlocked : String
    , chapter : ChapterId -> ChapterInfo
    , results : ResultTexts
    , theEnd : EndTexts
    , rabbitHole : RabbitHoleTexts
    , hall : HallTexts
    , drinkMe : DrinkMeTexts
    , pool : PoolTexts
    , caterpillar : CaterpillarTexts
    }


type alias ChapterInfo =
    { name : String
    , skill : String
    , lesson : String
    }


type alias ResultTexts =
    { title : String
    , next : String
    , replay : String
    , menu : String
    , keystrokes : String
    , fewestPossible : String
    , usedCount : String
    , accuracy : String
    , jarsCaught : String
    , bestStreak : String
    , wordsPerMinute : String
    , wrongDeletions : String
    , backspaces : String
    , practiceJars : String
    , yes : String
    , notYet : String
    }


type alias EndTexts =
    { title : String
    , tarts : Int -> String
    , teaser : String
    }


{-| Stage descriptions take the two keys as arguments, because the home row
differs between layouts (`;` on QWERTY, `ö` on QWERTZ, `m` on AZERTY).
-}
type alias RabbitHoleTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , indexFingers : String -> String -> String
    , middleFingers : String -> String -> String
    , ringFingers : String -> String -> String
    , littleFingers : String -> String -> String
    , allEight : String
    , stretch : String -> String -> String
    , stage : Int -> Int -> String
    , goalReached : String
    , practice : String
    , practiceLabel : String
    , speed : String
    , landing1 : String
    , landing2 : String
    }


type alias HallTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , keyFound : String
    , countLesson : String
    , locked : String
    , finale : String
    , findKey : String
    , goToCurtain : String
    , countTip : String
    , azertyTip : String
    , arrows : String
    }


type alias DrinkMeTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , finale : String
    , words : List String
    , sips : String
    , newKeys : String
    , bottle : String
    }


type alias PoolTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , finale : String
    , mouse : String
    , poem : String
    , cleanPoem : String
    , junk : String
    , flood : String
    , walkToJunk : String
    , pressW : String
    , pressD : String
    , removeAll : Int -> String
    , removeLine : String
    , goal : String
    , undoTip : String
    , wrongDeletion : String
    , idea : String
    }


type alias CaterpillarTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , finale1 : String
    , finale2 : String
    , gaps : String
    , answer : String
    , pressI : String
    , typeWord : String
    , pressEsc : String
    , fillAll : Int -> String
    , goal : String
    , tip : String
    , escTip : String
    }


forLanguage : Language -> Texts
forLanguage language =
    case language of
        English ->
            english

        German ->
            german


lines : List String -> String
lines =
    String.join "\n"



-- ENGLISH


english : Texts
english =
    { logo = { before = "Helix", small = "in", after = "Wonderland" }
    , tagline = "Learn the Helix editor and ten-finger typing, one chapter of Alice at a time."
    , menuHint = "j / k to choose · Enter to play"
    , menu = "Menu"
    , soundOn = "Sound on"
    , soundOff = "Sound off"
    , continueHint = "Enter ↵ to continue"
    , drawPictures = "Draw pictures for the game"
    , suggestChange = "Suggest a change to this screen"
    , footer = "Story and characters from Lewis Carroll's Alice's Adventures in Wonderland (1865), which is in the public domain. Helix is a free, open-source editor; this game is not affiliated with it."
    , keyboardLegend = "Each colour is one finger. Matching colours, matching finger."
    , space = "space"
    , nextKey = \key finger -> "Next key: " ++ key ++ " · " ++ finger
    , finger =
        \finger ->
            case finger of
                LeftLittle ->
                    "left little finger"

                LeftRing ->
                    "left ring finger"

                LeftMiddle ->
                    "left middle finger"

                LeftIndex ->
                    "left index finger"

                RightIndex ->
                    "right index finger"

                RightMiddle ->
                    "right middle finger"

                RightRing ->
                    "right ring finger"

                RightLittle ->
                    "right little finger"

                Thumb ->
                    "thumb"
    , speaker =
        \speaker ->
            case speaker of
                Narrator ->
                    "Narrator"

                Alice ->
                    "Alice"

                WhiteRabbit ->
                    "White Rabbit"

                Caterpillar ->
                    "Caterpillar"

                Mouse ->
                    "Mouse"
    , layoutDetected = \layout -> "Your keyboard looks like " ++ layout ++ ". Switched."
    , notYet = "That key belongs to a later chapter."
    , locked = "locked"
    , finishEarlierChapter = "Finish the chapter before this one first."
    , startHere = "start here"
    , unlockAll = "Unlock all chapters (for grown-ups)"
    , allUnlocked = "All chapters are unlocked"
    , chapter =
        \id ->
            case id of
                RabbitHole ->
                    { name = "Down the Rabbit Hole", skill = "Home row typing", lesson = "Fingers live on the home row." }

                HallOfDoors ->
                    { name = "The Hall of Doors", skill = "Helix: move with h j k l, counts", lesson = "h ← j ↓ k ↑ l →, and 5j walks five steps." }

                DrinkMe ->
                    { name = "Drink Me", skill = "Typing words, new keys e and i", lesson = "Words with e and i, thumb on the space bar." }

                PoolOfTears ->
                    { name = "The Pool of Tears", skill = "Helix: w e b select, d deletes, x lines, u undo", lesson = "Select first, then act: w e b x, then d. u undoes." }

                CaterpillarsAdvice ->
                    { name = "Advice from a Caterpillar", skill = "Helix: i and a insert, Esc returns", lesson = "i inserts before, a after, Esc goes back to NOR." }
    , results =
        { title = "Chapter complete!"
        , next = "Next"
        , replay = "Play again"
        , menu = "Menu"
        , keystrokes = "Keystrokes"
        , fewestPossible = "Fewest possible"
        , usedCount = "Used a count"
        , accuracy = "Accuracy"
        , jarsCaught = "Jars caught"
        , bestStreak = "Best streak"
        , wordsPerMinute = "Words per minute"
        , wrongDeletions = "Wrong deletions"
        , backspaces = "Backspaces"
        , practiceJars = "Practice jars"
        , yes = "yes"
        , notYet = "not yet"
        }
    , theEnd =
        { title = "The end, for now"
        , tarts = \count -> "You collected " ++ String.fromInt count ++ " of 15 tarts."
        , teaser = "Next in Wonderland: the Mad Tea-Party, where everyone moves one seat along at the same time. That is where Helix's multiple cursors come in."
        }
    , rabbitHole =
        { intro1 = "Alice was getting very tired of sitting by her sister with nothing to do. Then a White Rabbit with pink eyes ran close by her."
        , intro2 = "Oh dear! Oh dear! I shall be late!"
        , intro3 = "Down went Alice after it. The rabbit hole dipped suddenly down, like a very deep well. Jars float past the shelves as she falls."
        , intro4 = "Catch a jar by pressing the letter on its label. First, find home: left index finger on F, right index finger on J. Can you feel the little bumps?"
        , indexFingers = \a b -> a ++ " and " ++ b ++ ": your two index fingers. Don't look down, feel the bumps."
        , middleFingers = \a b -> a ++ " and " ++ b ++ ": your middle fingers, right next door."
        , ringFingers = \a b -> a ++ " and " ++ b ++ ": your ring fingers."
        , littleFingers = \a b -> a ++ " and " ++ b ++ ": your little fingers."
        , allEight = "All eight together. This is the home row. Your fingers live here."
        , stretch = \a b -> a ++ " and " ++ b ++ ": your index fingers stretch one key inward, then come home."
        , stage = \current total -> "Stage " ++ String.fromInt current ++ " of " ++ String.fromInt total
        , goalReached = "Goal reached! Keep catching to practise, or press Esc to land."
        , practice = "Practice with all keys: the jars get faster and faster. Press Esc to land."
        , practiceLabel = "Practice · all keys"
        , speed = "Speed"
        , landing1 = "Thump! Thump! Down Alice came upon a heap of sticks and dry leaves, and the fall was over."
        , landing2 = "Your fingers know the home row now. The keys h j k l live there too, and you will need them in the next chapter."
        }
    , hall =
        { intro1 = "Alice found herself in a long, low hall. There were doors all round it, but they were all locked."
        , intro2 = "Oh my ears and whiskers, how late it's getting!"
        , intro3 = "In Helix you move without the mouse and without the arrow keys. Your right hand already rests on the keys you need."
        , intro4 = "h moves left, j down, k up, l right. A trick: the j has a hook that hangs down. Find the tiny golden key."
        , keyFound = "A tiny golden key! It must open one of these doors."
        , countLesson = "A faster way to walk: type a number first. 8l walks eight steps right. Helix calls this a count. Try it on your way to the little curtain in the bottom-right corner."
        , locked = "The little door is locked. Where is that key?"
        , finale = "Behind the curtain was a little door about fifteen inches high. The key fitted! But Alice could not even get her head through the doorway."
        , findKey = "Find the tiny golden key."
        , goToCurtain = "Walk to the little curtain, bottom right."
        , countTip = "Type a number first, like 5j, to walk five steps."
        , azertyTip = "On AZERTY, digits need Shift: Shift+5, then j."
        , arrows = "Arrow keys are for rabbits in a hurry. Keep your fingers home: h j k l."
        }
    , drinkMe =
        { intro1 = "On the table stood a little bottle. Round its neck was a paper label with the words DRINK ME printed in large letters."
        , intro2 = "Each word you type is one sip. Type the word, then tap the space bar with your thumb."
        , intro3 = "Two new keys: e is your left middle finger reaching up one row, i is your right middle finger reaching up. Always come back home afterwards."
        , finale = "What a curious feeling! Alice was now only ten inches high, the right size for going through the little door."
        , words = [ "ask", "fall", "sad", "flask", "alas", "glad", "she", "like", "idea", "shelf", "hide", "lake" ]
        , sips = "Sips"
        , newKeys = "New keys: e and i. Middle fingers reach up."
        , bottle = "DRINK ME"
        }
    , pool =
        { intro1 = "Alice cried so much that her tears made a pool. Now the words of her poem float in it, and the pool keeps splashing extra words in."
        , intro2 = "I'll try if I know all the things I used to know. How doth the little..."
        , intro3 = "Here is the big Helix idea: first you select, then you act. Press w and Helix selects the next word. Press d and the selection is deleted."
        , intro4 = "Clean every splash out of the poem. e selects to the end of a word, b goes back one word. Deleted the wrong thing? u undoes it."
        , finale = "The poem is clean and the pool is calm. A Mouse swims up to lead everyone to the shore."
        , mouse = "Sit down, all of you, and listen to me! I'll soon make you dry enough!"
        , poem =
            lines
                [ "How doth the little splash crocodile"
                , "Improve splash his shining tail,"
                , "tears tears tears tears tears"
                , "And pour the waters of splash the Nile"
                , "On every golden scale! splash"
                ]
        , cleanPoem =
            lines
                [ "How doth the little crocodile"
                , "Improve his shining tail,"
                , "And pour the waters of the Nile"
                , "On every golden scale!"
                ]
        , junk = "splash"
        , flood = "tears"
        , walkToJunk = "Walk the cursor onto a \"splash\" with h j k l."
        , pressW = "Press w. The word lights up: that is a selection."
        , pressD = "Press d to delete what is selected."
        , removeAll = \left -> "Remove every \"splash\" (" ++ String.fromInt left ++ " left)."
        , removeLine = "A whole line of tears! x selects the line, then d."
        , goal = "The poem should read:"
        , undoTip = "Oops? u undoes the last change."
        , wrongDeletion = "A poem word went missing. Press u to undo."
        , idea = "Helix: select first, then act."
        }
    , caterpillar =
        { intro1 = "On the top of a large mushroom sat a large blue Caterpillar, with its arms folded."
        , intro2 = "Who are YOU?"
        , intro3 = "Some of the words are missing. Move to a gap and press i to start inserting. The mode in the status bar changes from NOR to INS."
        , intro4 = "In INS every key types letters, just like the typing chapters. Press Esc to go back to NOR. a inserts after the cursor instead of before it."
        , finale1 = "One side will make you grow taller, and the other side will make you grow shorter."
        , finale2 = "And with that, the Caterpillar got down off the mushroom and crawled away into the grass."
        , gaps =
            lines
                [ "\"Who are ?\" said the Caterpillar."
                , "\"I  know, sir, just at present.\""
                , "\"Keep your ,\" said the Caterpillar."
                ]
        , answer =
            lines
                [ "\"Who are you?\" said the Caterpillar."
                , "\"I hardly know, sir, just at present.\""
                , "\"Keep your temper,\" said the Caterpillar."
                ]
        , pressI = "Move the cursor onto the ? and press i. NOR turns into INS."
        , typeWord = "Type the missing word."
        , pressEsc = "Press Esc to go back to NOR."
        , fillAll = \filled -> "Fill all three gaps (" ++ String.fromInt filled ++ " of 3), then end in NOR."
        , goal = "What the Caterpillar really said:"
        , tip = "a inserts after the cursor. c deletes a selection and starts inserting."
        , escTip = "If Esc does nothing in your browser, Ctrl+[ works too."
        }
    }



-- GERMAN (Swiss spelling: ss, never ß)


german : Texts
german =
    { logo = { before = "Helix", small = "im", after = "Wunderland" }
    , tagline = "Lerne den Helix-Editor und das Zehnfingersystem, Kapitel für Kapitel mit Alice."
    , menuHint = "j / k zum Wählen · Enter zum Spielen"
    , menu = "Menü"
    , soundOn = "Ton an"
    , soundOff = "Ton aus"
    , continueHint = "Enter ↵ zum Weiterlesen"
    , drawPictures = "Bilder fürs Spiel zeichnen"
    , suggestChange = "Änderung für diese Seite vorschlagen"
    , footer = "Geschichte und Figuren aus Lewis Carrolls Alice im Wunderland (1865), gemeinfrei. Helix ist ein freier Open-Source-Editor; dieses Spiel gehört nicht zum Helix-Projekt."
    , keyboardLegend = "Jede Farbe ist ein Finger. Gleiche Farbe, gleicher Finger."
    , space = "Leertaste"
    , nextKey = \key finger -> "Nächste Taste: " ++ key ++ " · " ++ finger
    , finger =
        \finger ->
            case finger of
                LeftLittle ->
                    "linker kleiner Finger"

                LeftRing ->
                    "linker Ringfinger"

                LeftMiddle ->
                    "linker Mittelfinger"

                LeftIndex ->
                    "linker Zeigefinger"

                RightIndex ->
                    "rechter Zeigefinger"

                RightMiddle ->
                    "rechter Mittelfinger"

                RightRing ->
                    "rechter Ringfinger"

                RightLittle ->
                    "rechter kleiner Finger"

                Thumb ->
                    "Daumen"
    , speaker =
        \speaker ->
            case speaker of
                Narrator ->
                    "Erzähler"

                Alice ->
                    "Alice"

                WhiteRabbit ->
                    "Weisses Kaninchen"

                Caterpillar ->
                    "Raupe"

                Mouse ->
                    "Maus"
    , layoutDetected = \layout -> "Deine Tastatur sieht nach " ++ layout ++ " aus. Umgestellt."
    , notYet = "Diese Taste kommt erst in einem späteren Kapitel."
    , locked = "gesperrt"
    , finishEarlierChapter = "Schliess zuerst das Kapitel davor ab."
    , startHere = "hier starten"
    , unlockAll = "Alle Kapitel freischalten (für Erwachsene)"
    , allUnlocked = "Alle Kapitel sind frei"
    , chapter =
        \id ->
            case id of
                RabbitHole ->
                    { name = "Hinunter in den Kaninchenbau", skill = "Grundreihe tippen", lesson = "Die Finger wohnen auf der Grundreihe." }

                HallOfDoors ->
                    { name = "Der Saal der Türen", skill = "Helix: bewegen mit h j k l, Counts", lesson = "h ← j ↓ k ↑ l →, und 5j geht fünf Schritte." }

                DrinkMe ->
                    { name = "Trink mich", skill = "Wörter tippen, neue Tasten e und i", lesson = "Wörter mit e und i, Daumen auf der Leertaste." }

                PoolOfTears ->
                    { name = "Der Tränenteich", skill = "Helix: w e b wählen, d löscht, x Zeilen, u zurück", lesson = "Erst auswählen, dann handeln: w e b x, dann d. u macht rückgängig." }

                CaterpillarsAdvice ->
                    { name = "Die Raupe gibt Rat", skill = "Helix: i und a einfügen, Esc zurück", lesson = "i fügt davor ein, a dahinter, Esc zurück zu NOR." }
    , results =
        { title = "Kapitel geschafft!"
        , next = "Weiter"
        , replay = "Nochmal"
        , menu = "Menü"
        , keystrokes = "Tastendrücke"
        , fewestPossible = "Am wenigsten möglich"
        , usedCount = "Count benutzt"
        , accuracy = "Genauigkeit"
        , jarsCaught = "Gläser gefangen"
        , bestStreak = "Beste Serie"
        , wordsPerMinute = "Wörter pro Minute"
        , wrongDeletions = "Falsch gelöscht"
        , backspaces = "Rücktaste"
        , practiceJars = "Übungsgläser"
        , yes = "ja"
        , notYet = "noch nicht"
        }
    , theEnd =
        { title = "Ende, fürs Erste"
        , tarts = \count -> "Du hast " ++ String.fromInt count ++ " von 15 Törtchen gesammelt."
        , teaser = "Als Nächstes im Wunderland: die verrückte Teegesellschaft, wo alle gleichzeitig einen Platz weiterrücken. Genau dafür hat Helix mehrere Cursor."
        }
    , rabbitHole =
        { intro1 = "Alice sass bei ihrer Schwester am Ufer und langweilte sich. Da rannte ein Weisses Kaninchen mit rosa Augen dicht an ihr vorbei."
        , intro2 = "Oje, oje! Ich komme zu spät!"
        , intro3 = "Alice sprang ihm nach, hinunter in den Kaninchenbau. Der fiel plötzlich steil ab, wie ein tiefer Brunnen. An den Regalen schweben Gläser vorbei."
        , intro4 = "Fang ein Glas, indem du den Buchstaben auf dem Etikett drückst. Zuerst die Grundstellung: linker Zeigefinger auf F, rechter Zeigefinger auf J. Spürst du die kleinen Huckel?"
        , indexFingers = \a b -> a ++ " und " ++ b ++ ": deine Zeigefinger. Nicht hinschauen, die Huckel fühlen."
        , middleFingers = \a b -> a ++ " und " ++ b ++ ": die Mittelfinger, gleich daneben."
        , ringFingers = \a b -> a ++ " und " ++ b ++ ": die Ringfinger."
        , littleFingers = \a b -> a ++ " und " ++ b ++ ": die kleinen Finger."
        , allEight = "Alle acht zusammen. Das ist die Grundreihe. Hier wohnen deine Finger."
        , stretch = \a b -> a ++ " und " ++ b ++ ": die Zeigefinger strecken sich eine Taste nach innen und kommen zurück."
        , stage = \current total -> "Stufe " ++ String.fromInt current ++ " von " ++ String.fromInt total
        , goalReached = "Ziel erreicht! Fang weiter, um zu üben, oder drück Esc zum Landen."
        , practice = "Üben mit allen Tasten: die Gläser werden immer schneller. Esc zum Landen."
        , practiceLabel = "Üben · alle Tasten"
        , speed = "Tempo"
        , landing1 = "Plumps! Plumps! Alice landete auf einem Haufen Zweige und trockener Blätter, und der Fall war vorbei."
        , landing2 = "Deine Finger kennen jetzt die Grundreihe. Auch h j k l wohnen dort, und die brauchst du im nächsten Kapitel."
        }
    , hall =
        { intro1 = "Alice stand in einem langen, niedrigen Saal. Ringsum waren Türen, aber alle waren verschlossen."
        , intro2 = "Bei meinen Ohren und Schnurrhaaren, wie spät es schon ist!"
        , intro3 = "In Helix bewegst du dich ohne Maus und ohne Pfeiltasten. Deine rechte Hand liegt schon auf den richtigen Tasten."
        , intro4 = "h geht nach links, j nach unten, k nach oben, l nach rechts. Merkhilfe: das j hat unten einen Haken. Finde den winzigen goldenen Schlüssel."
        , keyFound = "Ein winziger goldener Schlüssel! Er passt bestimmt in eine dieser Türen."
        , countLesson = "Schneller laufen: tippe zuerst eine Zahl. 8l geht acht Schritte nach rechts. Helix nennt das einen Count. Probier es auf dem Weg zum kleinen Vorhang unten rechts."
        , locked = "Die kleine Tür ist verschlossen. Wo ist der Schlüssel?"
        , finale = "Hinter dem Vorhang war eine kleine Tür, kaum vierzig Zentimeter hoch. Der Schlüssel passte! Aber Alice bekam nicht einmal den Kopf durch die Öffnung."
        , findKey = "Finde den winzigen goldenen Schlüssel."
        , goToCurtain = "Geh zum kleinen Vorhang unten rechts."
        , countTip = "Tippe zuerst eine Zahl, zum Beispiel 5j, für fünf Schritte."
        , azertyTip = "Auf AZERTY brauchen Ziffern die Umschalttaste: Umschalt+5, dann j."
        , arrows = "Pfeiltasten sind für eilige Kaninchen. Lass die Finger zu Hause: h j k l."
        }
    , drinkMe =
        { intro1 = "Auf dem Tisch stand ein Fläschchen. Um seinen Hals hing ein Zettel mit grossen Buchstaben: TRINK MICH."
        , intro2 = "Jedes Wort, das du tippst, ist ein Schluck. Tippe das Wort und drücke dann mit dem Daumen die Leertaste."
        , intro3 = "Zwei neue Tasten: e ist der linke Mittelfinger, eine Reihe nach oben. i ist der rechte Mittelfinger, eine Reihe nach oben. Danach immer zurück nach Hause."
        , finale = "Was für ein seltsames Gefühl! Alice war nur noch fünfundzwanzig Zentimeter gross, genau richtig für die kleine Tür."
        , words = [ "das", "ja", "als", "lass", "fass", "glas", "eis", "alle", "leise", "seide", "held", "ideal" ]
        , sips = "Schlucke"
        , newKeys = "Neue Tasten: e und i. Die Mittelfinger greifen nach oben."
        , bottle = "TRINK MICH"
        }
    , pool =
        { intro1 = "Alice hatte so viel geweint, dass ihre Tränen einen Teich bildeten. Jetzt schwimmt ihr Gedicht darin, und der Teich spritzt immer neue Wörter hinein."
        , intro2 = "Mal sehen, ob ich noch alles weiss, was ich früher wusste. Wie putzt das kleine..."
        , intro3 = "Hier ist die grosse Idee von Helix: erst auswählen, dann handeln. Drücke w und Helix wählt das nächste Wort aus. Drücke d und die Auswahl wird gelöscht."
        , intro4 = "Fisch jedes platsch aus dem Gedicht. e wählt bis zum Wortende, b geht ein Wort zurück. Das Falsche gelöscht? u macht es rückgängig."
        , finale = "Das Gedicht ist sauber und der Teich wird still. Eine Maus schwimmt heran, um alle ans Ufer zu führen."
        , mouse = "Setzt euch alle hin und hört mir zu! Ich mache euch gleich trocken genug!"
        , poem =
            lines
                [ "Wie putzt das platsch kleine Krokodil"
                , "den Schwanz platsch so blank und fein,"
                , "tränen tränen tränen tränen tränen"
                , "und giesst das Wasser platsch aus dem Nil"
                , "auf jede Schuppe ein! platsch"
                ]
        , cleanPoem =
            lines
                [ "Wie putzt das kleine Krokodil"
                , "den Schwanz so blank und fein,"
                , "und giesst das Wasser aus dem Nil"
                , "auf jede Schuppe ein!"
                ]
        , junk = "platsch"
        , flood = "tränen"
        , walkToJunk = "Geh mit h j k l auf ein \"platsch\"."
        , pressW = "Drücke w. Das Wort leuchtet auf: das ist eine Auswahl."
        , pressD = "Drücke d, um die Auswahl zu löschen."
        , removeAll = \left -> "Entferne jedes \"platsch\" (noch " ++ String.fromInt left ++ ")."
        , removeLine = "Eine ganze Zeile Tränen! x wählt die Zeile, dann d."
        , goal = "So soll das Gedicht lauten:"
        , undoTip = "Hoppla? u macht die letzte Änderung rückgängig."
        , wrongDeletion = "Ein Wort aus dem Gedicht fehlt jetzt. Drücke u."
        , idea = "Helix: erst auswählen, dann handeln."
        }
    , caterpillar =
        { intro1 = "Oben auf einem grossen Pilz sass eine grosse blaue Raupe mit verschränkten Armen."
        , intro2 = "Wer bist DU?"
        , intro3 = "Ein paar Wörter fehlen. Geh zu einer Lücke und drücke i, um einzufügen. In der Statusleiste wird aus NOR dann INS."
        , intro4 = "In INS schreibt jede Taste Buchstaben, wie in den Tipp-Kapiteln. Mit Esc geht es zurück zu NOR. Mit a fügst du hinter dem Cursor ein statt davor."
        , finale1 = "Die eine Seite lässt dich wachsen, die andere Seite lässt dich schrumpfen."
        , finale2 = "Und damit kletterte die Raupe vom Pilz und kroch davon ins Gras."
        , gaps =
            lines
                [ "\"Wer bist ?\" fragte die Raupe."
                , "\"Das  ich kaum sagen, mein Herr.\""
                , "\"Bleib ,\" sagte die Raupe."
                ]
        , answer =
            lines
                [ "\"Wer bist du?\" fragte die Raupe."
                , "\"Das kann ich kaum sagen, mein Herr.\""
                , "\"Bleib ruhig,\" sagte die Raupe."
                ]
        , pressI = "Geh mit dem Cursor auf das ? und drücke i. Aus NOR wird INS."
        , typeWord = "Tippe das fehlende Wort."
        , pressEsc = "Drücke Esc, um zurück zu NOR zu kommen."
        , fillAll = \filled -> "Füll alle drei Lücken (" ++ String.fromInt filled ++ " von 3) und ende in NOR."
        , goal = "Was die Raupe wirklich sagt:"
        , tip = "a fügt hinter dem Cursor ein. c löscht eine Auswahl und beginnt mit dem Einfügen."
        , escTip = "Wenn Esc im Browser nicht geht, funktioniert auch Strg+[."
        }
    }
