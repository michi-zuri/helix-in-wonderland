module Texts exposing
    ( Texts, ChapterInfo, ResultTexts, EndTexts
    , RabbitHoleTexts, HallTexts, DrinkMeTexts, CuriouserTexts, PoolTexts, CaterpillarTexts
    , forLanguage
    )

{-| Every word the player reads, per language.

`Texts` is a record, so each language must fill in every field: a missing
German line is a compile error, not a blank on screen. German follows Swiss
spelling (ss instead of ß).

Story lines come from Lewis Carroll's 1865 book (public domain) or are newly
written for the game; the German versions are our own translations.

@docs Texts, ChapterInfo, ResultTexts, EndTexts
@docs RabbitHoleTexts, HallTexts, DrinkMeTexts, CuriouserTexts, PoolTexts, CaterpillarTexts
@docs forLanguage

-}

import Chapter.Id exposing (ChapterId(..))
import Language exposing (Language(..))
import Speaker exposing (Speaker(..))


type alias Texts =
    { logo : { before : String, small : String, after : String }
    , tagline : String
    , menuHint : String
    , menu : String
    , soundOn : String
    , soundOff : String
    , fullscreen : String
    , exitFullscreen : String
    , escapeKeys : { title : String, explain : String, keep : String -> String, chosen : String -> String }
    , escapeKeysWork : String -> String
    , continueHint : String
    , footer : String
    , drawPictures : String
    , suggestChange : String
    , speaker : Speaker -> String
    , layoutDetected : String -> String
    , notYet : String
    , locked : String
    , finishEarlierChapter : String
    , startHere : String
    , unlockAll : String
    , allUnlocked : String
    , difficulty : { easy : String, challenge : String, mad : String }
    , chapter : ChapterId -> ChapterInfo
    , results : ResultTexts
    , theEnd : EndTexts
    , rabbitHole : RabbitHoleTexts
    , hall : HallTexts
    , drinkMe : DrinkMeTexts
    , curiouser : CuriouserTexts
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
    , difficulty : String
    , noTart : String
    , noTartHint : String
    , yes : String
    , notYet : String
    }


type alias EndTexts =
    { title : String
    , tarts : { collected : Int, possible : Int } -> String
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
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , allKeys : String
    , allKeysLabel : String
    , toGround : String
    , letGo : String
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
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , doorFound : String
    , doorRule : String
    , doorsTip : String
    }


{-| Each word list is one hidden sentence. Mad's made-up words sound like a
sentence when read aloud. Words may only use the letters learned so far:
the home row, plus `e` and `i` (a test checks this).
-}
type alias DrinkMeTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , finale : String
    , easyWords : List String
    , challengeWords : List String
    , madWords : List String
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , fromMemory : String
    , tooSlow : String
    , startedOver : String
    , sips : String
    , newKeys : String
    , bottle : String
    }


{-| The currant letters, in the order they hop off the cake, spell a
hidden sentence per difficulty; the game never explains it.
-}
type alias CuriouserTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , finale1 : String
    , finale2 : String
    , easyLetters : String
    , challengeLetters : String
    , madLetters : String
    , cake : String
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , lineEnds : String
    , fileEnds : String
    , realLines : String
    , lastChange : String
    , allCheeky : String
    , cheshireAppears : String
    , cheshireSays : String
    , newTrick : String -> String
    , escaped : String
    , currants : String
    , squashed : String
    }


{-| Chapter 5's poems, one per difficulty, all cleaned into `cleanPoem` by
deleting words and lines. The words to delete carry a hidden meaning that
the game never explains: on easy they spell a sentence, the tide on
challenge splashes one in word by word (`tideWords`), and on mad they are
Isaac Watts' busy-bee poem, which Alice mixes up with the crocodile in the
book.

`easyBest` and `madBest` are the keystrokes of our own best way to clean
those poems, shown on the results card as a number to beat. A test plays
that way through the chapter and checks the number.

-}
type alias PoolTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , finale : String
    , mouse : String
    , easyPoem : String
    , challengePoem : String
    , madPoem : String
    , cleanPoem : String
    , tideWords : List String
    , easyBest : Int
    , madBest : Int
    , ourBest : String
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , hintFind : String
    , hintDelete : String
    , hintLines : Int -> String
    , hintAnyLine : String
    , hintUndo : String
    , hintCount : String
    , hintLineStart : String
    , wordsLeft : Int -> String
    , goal : String
    , wrongDeletion : String
    , idea : String
    }


{-| Chapter 6's texts. Easy has gaps to fill (`gaps`, `answer`). Challenge
mixes missing words (`i`), a word missing at a line's end (`a`) and wrong
words (`w` then `c`). Mad is the poem Alice recites to the Caterpillar in
the book, "You are old, Father William", as she gets it wrong: wrong words,
missing words, line endings and a line that doesn't belong (`x` then `d`).
`madBest` is our own best keystroke count for it; a test plays that way.
-}
type alias CaterpillarTexts =
    { intro1 : String
    , intro2 : String
    , intro3 : String
    , intro4 : String
    , finale1 : String
    , finale2 : String
    , gaps : String
    , answer : String
    , challengeStart : String
    , challengeAnswer : String
    , madStart : String
    , madAnswer : String
    , madBest : Int
    , chooseTitle : String
    , chooseHint : String
    , easyInfo : String
    , challengeInfo : String
    , madInfo : String
    , hintFind : String
    , hintChange : String
    , hintAppend : String
    , hintTyping : String
    , ourBest : String
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
    , fullscreen = "Fullscreen"
    , exitFullscreen = "Leave fullscreen"
    , escapeKeys =
        { title = "Your escape keys"
        , explain = "In Helix, Esc takes you from Insert mode back to Normal mode. But in fullscreen the browser uses Esc to leave fullscreen. So choose two keys that do it instead, for example ;; or ,, and type them now."
        , keep = \keys -> "Enter keeps " ++ keys
        , chosen = \keys -> "Your escape keys: " ++ keys
        }
    , escapeKeysWork = \keys -> "Your escape keys " ++ keys ++ " work like Esc."
    , continueHint = "Enter ↵ to continue"
    , drawPictures = "Draw pictures for the game"
    , suggestChange = "Suggest a change to this screen"
    , footer = "Story and characters from Lewis Carroll's Alice's Adventures in Wonderland (1865), which is in the public domain. Helix is a free, open-source editor; this game is not affiliated with it."
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

                CheshireCat ->
                    "Cheshire Cat"
    , layoutDetected = \layout -> "Your keyboard looks like " ++ layout ++ ". Switched."
    , notYet = "That key belongs to a later chapter."
    , locked = "locked"
    , finishEarlierChapter = "Finish the chapter before this one first."
    , startHere = "start here"
    , unlockAll = "Unlock all chapters (for grown-ups)"
    , allUnlocked = "All chapters are unlocked"
    , difficulty = { easy = "Easy", challenge = "Challenge", mad = "Mad" }
    , chapter =
        \id ->
            case id of
                RabbitHole ->
                    { name = "Down the Rabbit Hole", skill = "Home row typing", lesson = "Fingers live on the home row." }

                HallOfDoors ->
                    { name = "The Hall of Doors", skill = "Helix: move with h j k l, counts", lesson = "h ← j ↓ k ↑ l →, and 5j walks five steps." }

                DrinkMe ->
                    { name = "Drink Me", skill = "Typing words, new keys e and i", lesson = "Words with e and i, thumb on the space bar." }

                Curiouser ->
                    { name = "Curiouser and Curiouser", skill = "Helix jumps: gh gl gg ge gj gk g.", lesson = "Squash the currants with Helix's g jumps." }

                PoolOfTears ->
                    { name = "The Pool of Tears", skill = "Helix: w x select, d deletes, u undo, counts", lesson = "Select first, then act: w or x, then d. u undoes." }

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
        , difficulty = "Difficulty"
        , noTart = "No tart this time"
        , noTartHint = "One tart opens the next chapter."
        , yes = "yes"
        , notYet = "not yet"
        }
    , theEnd =
        { title = "The end, for now"
        , tarts = \{ collected, possible } -> "You collected " ++ String.fromInt collected ++ " of " ++ String.fromInt possible ++ " tarts."
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
        , chooseTitle = "How fast do you want to fall?"
        , chooseHint = "j / k to choose · Enter to jump"
        , easyInfo = "A gentle fall. You land after the basic jars."
        , challengeInfo = "Quicker jars, and a few seconds more."
        , madInfo = "We're all mad here: nearly 30 seconds of fast jars, no mistakes."
        , allKeys = "All keys now. Keep catching to reach the ground."
        , allKeysLabel = "All keys"
        , toGround = "To the ground"
        , letGo = "Esc lets go: Alice lands where she is. Mistakes lift her up again."
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
        , chooseTitle = "Which hall do you want to explore?"
        , chooseHint = "j / k to choose · Enter to go in"
        , easyInfo = "The hall as it is: find the key, then the little curtain."
        , challengeInfo = "Things dropped all over the floor: weave your way round them, turning in every direction."
        , madInfo = "Heaps of dropped things cut the hall into parts. Only the doors lead on, and they keep changing where they go."
        , doorFound = "Alice opened the door and stepped out of another door, somewhere else in the hall!"
        , doorRule = "The doors in this hall play tricks. The door you just came out of takes you back. Every other door may lead somewhere new each time."
        , doorsTip = "The way back always works. All the other doors change where they lead whenever you go through one."
        }
    , drinkMe =
        { intro1 = "On the table stood a little bottle. Round its neck was a paper label with the words DRINK ME printed in large letters."
        , intro2 = "Each word you type is one sip. Look at the word, then type it from memory, and tap the space bar with your thumb."
        , intro3 = "Two new keys: e is your left middle finger reaching up one row, i is your right middle finger reaching up. Always come back home afterwards."
        , finale = "What a curious feeling! Alice was now only ten inches high, the right size for going through the little door."
        , easyWords = [ "alas", "she", "feels", "as", "if", "she", "shall", "fade", "like", "a", "sad", "leaf" ]
        , challengeWords = [ "she", "glided", "aside", "as", "seashells", "salads", "ladles", "flasks", "giggled", "sighed", "faded" ]
        , madWords = [ "alaasaleesiss", "ashaiashaleef", "sheesaighsshee", "shallhaidasafish", "afishalaas", "fishesfeelsaad" ]
        , chooseTitle = "How big a sip?"
        , chooseHint = "j / k to choose · Enter to drink"
        , easyInfo = "Short words."
        , challengeInfo = "Longer words."
        , madInfo = "Very long made-up words."
        , fromMemory = "Read the word first. It hides as soon as you start typing, and a pause in the middle of a word starts it over. Between words, take all the time you need."
        , tooSlow = "Too slow! The word starts over."
        , startedOver = "Started over"
        , sips = "Sips"
        , newKeys = "New keys: e and i. Middle fingers reach up."
        , bottle = "DRINK ME"
        }
    , curiouser =
        { intro1 = "Soon Alice's eye fell on a little glass box under the table. Inside was a very small cake, with the words EAT ME beautifully marked in currants."
        , intro2 = "Well, I'll eat it! If it makes me grow larger, I can reach the key."
        , intro3 = "But as she lifted the lid, the currant letters hopped off the cake and scattered over the crumbs on the table!"
        , intro4 = "Squash every currant before it rolls away: move the cursor onto it. Helix has jumps that start with g. They will come in handy."
        , finale1 = "Alice ate the cake to the very last crumb. \"Curiouser and curiouser!\" she cried. She was opening out like the largest telescope that ever was."
        , finale2 = "Good-bye, feet!"
        , easyLetters = "EAT ME AND GROW TALL"
        , challengeLetters = "OPENING OUT LIKE A TELESCOPE"
        , madLetters = "GOOD BYE FEET WHO WILL PUT ON YOUR SHOES"
        , cake = "EAT ME"
        , chooseTitle = "How quick are the currants?"
        , chooseHint = "j / k to choose · Enter to start"
        , easyInfo = "Slow currants, one at a time."
        , challengeInfo = "Quick currants."
        , madInfo = "Very quick currants, two at once at the end."
        , lineEnds = "A currant at the end of your line! g then l jumps to the end of the line, g then h to its start."
        , fileEnds = "A currant on the first or the last line! g then g jumps to the very first line, g then e to the last."
        , realLines = "Long lines go on over more rows (↪). j moves one row; g then j jumps to the next real line, g then k to the one before."
        , lastChange = "Cheeky currants are shy: once you get onto their line or column, they run back to where you squashed the last one. g then . jumps to your last change."
        , allCheeky = "Every currant is cheeky now! Get onto its line or column, then catch it with g then ."
        , cheshireAppears = "Then, slowly, a grin appeared in the air above the table, and after it, the rest of a Cat."
        , cheshireSays = "Every currant caught with gw, and not one slip! We're all mad here. You must be too, or you wouldn't have come."
        , newTrick = \keys -> "New trick: " ++ keys
        , escaped = "A currant rolled away!"
        , currants = "Currants"
        , squashed = "Squashed"
        }
    , pool =
        { intro1 = "Alice cried so much that her tears made a pool. Now the words of her poem float in it, and the pool keeps splashing extra words in."
        , intro2 = "I'll try if I know all the things I used to know. How doth the little..."
        , intro3 = "Here is the big Helix idea: first you select, then you act. Press w and Helix selects the next word. Press d and the selection is deleted."
        , intro4 = "Clean the poem: delete every word that doesn't belong. Deleted the wrong thing? u undoes it."
        , finale = "The poem is clean and the pool is calm. A Mouse swims up to lead everyone to the shore."
        , mouse = "Sit down, all of you, and listen to me! I'll soon make you dry enough!"
        , easyPoem =
            lines
                [ "How doth the little SWIM crocodile"
                , "Improve his TO shining tail,"
                , "tears tears tears tears tears"
                , "tears tears tears tears tears"
                , "And pour the THE waters of the Nile"
                , "On every golden scale! SHORE"
                ]
        , challengePoem =
            lines
                [ "How doth the LOOK little crocodile"
                , "Improve his shining tail, OUT"
                , "tears tears tears tears tears"
                , "tears tears tears tears tears"
                , "tears tears tears tears tears"
                , "And pour the waters of the Nile"
                , "On every golden scale!"
                ]
        , madPoem =
            lines
                [ "How doth the little busy crocodile"
                , "Improve his shining golden tail,"
                , "And gather honey all the day"
                , "From every opening flower!"
                , "And pour the honey waters of the Nile"
                , "On every opening golden scale!"
                ]
        , cleanPoem =
            lines
                [ "How doth the little crocodile"
                , "Improve his shining tail,"
                , "And pour the waters of the Nile"
                , "On every golden scale!"
                ]
        , tideWords = [ "A", "MOUSE", "IS", "COMING", "TO", "HELP" ]
        , easyBest = 19
        , madBest = 18
        , ourBest = "Our best"
        , chooseTitle = "How wild is the pool?"
        , chooseHint = "j / k to choose · Enter to swim"
        , easyInfo = "Odd words in capitals."
        , challengeInfo = "The pool keeps splashing new words in."
        , madInfo = "Some words almost belong. Read carefully!"
        , hintFind = "Find a word that doesn't belong. Walk with h j k l, or press w: it selects the next word."
        , hintDelete = "Selected! d deletes the selection."
        , hintLines =
            \count ->
                if count == 1 then
                    "This whole line doesn't belong. x selects the line."

                else
                    "These " ++ String.fromInt count ++ " lines don't belong. " ++ String.fromInt count ++ "x selects " ++ String.fromInt count ++ " lines at once."
        , hintAnyLine = "x selects a whole line. A number first selects more lines: 2x."
        , hintUndo = "A word of the poem went missing. u undoes the last change."
        , hintCount = "A number first repeats a key: 3w selects the third word ahead."
        , hintLineStart = "A shortcut: g, then h, jumps to the start of the line."
        , wordsLeft = \count -> "Words to delete: " ++ String.fromInt count
        , goal = "The poem should read:"
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
        , challengeStart =
            lines
                [ "\"What do you mean by this?\" said the Caterpillar sternly."
                , "\"Explain !\""
                , "\"I can't explain myself, I'm afraid, sir,\" said"
                , "\"Keep your hat,\" said the Caterpillar."
                ]
        , challengeAnswer =
            lines
                [ "\"What do you mean by that?\" said the Caterpillar sternly."
                , "\"Explain yourself!\""
                , "\"I can't explain myself, I'm afraid, sir,\" said Alice"
                , "\"Keep your temper,\" said the Caterpillar."
                ]
        , madStart =
            lines
                [ "\"You are young, Father William,\" the old man said,"
                , "\"And your hair has become white;"
                , "And yet you incessantly sit on your hat -"
                , "Do you think, at your age, it is right?\""
                , "And the Caterpillar smoked his long pipe."
                , "\"In my youth,\" Father William replied to his"
                , "\"I feared it might tickle the brain;"
                , "But, now that I'm sure I have none,"
                , "Why, I do it again and"
                ]
        , madAnswer =
            lines
                [ "\"You are old, Father William,\" the young man said,"
                , "\"And your hair has become very white;"
                , "And yet you incessantly stand on your head -"
                , "Do you think, at your age, it is right?\""
                , "\"In my youth,\" Father William replied to his son,"
                , "\"I feared it might injure the brain;"
                , "But, now that I'm perfectly sure I have none,"
                , "Why, I do it again and again.\""
                ]
        , madBest = 101
        , chooseTitle = "How grumpy is the Caterpillar?"
        , chooseHint = "j / k to choose · Enter to start"
        , easyInfo = "Fill the gaps."
        , challengeInfo = "Gaps, wrong words and line endings."
        , madInfo = "Alice recites a whole poem, all wrong."
        , hintFind = "Compare with the goal. A word missing: i inserts before the cursor. A wrong word: select it with w, then c. A line that doesn't belong: x, then d."
        , hintChange = "Selected! c deletes the selection and starts inserting: type the right word."
        , hintAppend = "At the end of a line, a inserts after the cursor (gl gets you there)."
        , hintTyping = "Type, then go back to NOR with Esc."
        , ourBest = "Our best"
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
    , fullscreen = "Vollbild"
    , exitFullscreen = "Vollbild verlassen"
    , escapeKeys =
        { title = "Deine Escape-Tasten"
        , explain = "In Helix bringt dich Esc vom Einfügemodus zurück in den Normalmodus. Im Vollbild verlässt der Browser mit Esc aber das Vollbild. Wähle darum zwei Tasten, die das stattdessen tun, zum Beispiel ;; oder ,, und tippe sie jetzt."
        , keep = \keys -> "Enter behält " ++ keys
        , chosen = \keys -> "Deine Escape-Tasten: " ++ keys
        }
    , escapeKeysWork = \keys -> "Deine Escape-Tasten " ++ keys ++ " wirken wie Esc."
    , continueHint = "Enter ↵ zum Weiterlesen"
    , drawPictures = "Bilder fürs Spiel zeichnen"
    , suggestChange = "Änderung für diese Seite vorschlagen"
    , footer = "Geschichte und Figuren aus Lewis Carrolls Alice im Wunderland (1865), gemeinfrei. Helix ist ein freier Open-Source-Editor; dieses Spiel gehört nicht zum Helix-Projekt."
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

                CheshireCat ->
                    "Grinsekatze"
    , layoutDetected = \layout -> "Deine Tastatur sieht nach " ++ layout ++ " aus. Umgestellt."
    , notYet = "Diese Taste kommt erst in einem späteren Kapitel."
    , locked = "gesperrt"
    , finishEarlierChapter = "Schliess zuerst das Kapitel davor ab."
    , startHere = "hier starten"
    , unlockAll = "Alle Kapitel freischalten (für Erwachsene)"
    , allUnlocked = "Alle Kapitel sind frei"
    , difficulty = { easy = "Leicht", challenge = "Herausforderung", mad = "Verrückt" }
    , chapter =
        \id ->
            case id of
                RabbitHole ->
                    { name = "Hinunter in den Kaninchenbau", skill = "Grundreihe tippen", lesson = "Die Finger wohnen auf der Grundreihe." }

                HallOfDoors ->
                    { name = "Der Saal der Türen", skill = "Helix: bewegen mit h j k l, Counts", lesson = "h ← j ↓ k ↑ l →, und 5j geht fünf Schritte." }

                DrinkMe ->
                    { name = "Trink mich", skill = "Wörter tippen, neue Tasten e und i", lesson = "Wörter mit e und i, Daumen auf der Leertaste." }

                Curiouser ->
                    { name = "Immer merkwürdiger", skill = "Helix-Sprünge: gh gl gg ge gj gk g.", lesson = "Zerdrück die Korinthen mit den g-Sprüngen von Helix." }

                PoolOfTears ->
                    { name = "Der Tränenteich", skill = "Helix: w x wählen, d löscht, u zurück, Zahlen", lesson = "Erst auswählen, dann handeln: w oder x, dann d. u macht rückgängig." }

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
        , difficulty = "Schwierigkeit"
        , noTart = "Kein Törtchen diesmal"
        , noTartHint = "Mit einem Törtchen geht das nächste Kapitel auf."
        , yes = "ja"
        , notYet = "noch nicht"
        }
    , theEnd =
        { title = "Ende, fürs Erste"
        , tarts = \{ collected, possible } -> "Du hast " ++ String.fromInt collected ++ " von " ++ String.fromInt possible ++ " Törtchen gesammelt."
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
        , chooseTitle = "Wie schnell willst du fallen?"
        , chooseHint = "j / k zum Wählen · Enter zum Springen"
        , easyInfo = "Gemütlich fallen. Nach den Grundgläsern bist du unten."
        , challengeInfo = "Flottere Gläser und ein paar Sekunden länger."
        , madInfo = "Wir sind hier alle verrückt: fast 30 Sekunden schnelle Gläser ohne Fehler."
        , allKeys = "Jetzt alle Tasten. Fang weiter, bis du unten bist."
        , allKeysLabel = "Alle Tasten"
        , toGround = "Bis zum Boden"
        , letGo = "Esc lässt los: Alice landet, wo sie gerade ist. Fehler heben sie wieder hoch."
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
        , chooseTitle = "Welchen Saal willst du erkunden?"
        , chooseHint = "j / k zum Wählen · Enter zum Hineingehen"
        , easyInfo = "Der Saal, wie er ist: finde den Schlüssel, dann den kleinen Vorhang."
        , challengeInfo = "Überall liegen heruntergefallene Sachen: Schlängle dich drum herum, mit Kurven in alle Richtungen."
        , madInfo = "Haufen von heruntergefallenen Sachen teilen den Saal. Nur die Türen führen weiter, und sie ändern ständig, wohin."
        , doorFound = "Alice öffnete die Tür und trat aus einer anderen Tür heraus, ganz woanders im Saal!"
        , doorRule = "Die Türen in diesem Saal spielen Streiche. Die Tür, aus der du gerade kommst, bringt dich zurück. Jede andere Tür kann jedes Mal woandershin führen."
        , doorsTip = "Der Weg zurück klappt immer. Alle anderen Türen ändern, wohin sie führen, sobald du durch eine gehst."
        }
    , drinkMe =
        { intro1 = "Auf dem Tisch stand ein Fläschchen. Um seinen Hals hing ein Zettel mit grossen Buchstaben: TRINK MICH."
        , intro2 = "Jedes Wort, das du tippst, ist ein Schluck. Schau dir das Wort an, tippe es aus dem Gedächtnis und drücke dann mit dem Daumen die Leertaste."
        , intro3 = "Zwei neue Tasten: e ist der linke Mittelfinger, eine Reihe nach oben. i ist der rechte Mittelfinger, eine Reihe nach oben. Danach immer zurück nach Hause."
        , finale = "Was für ein seltsames Gefühl! Alice war nur noch fünfundzwanzig Zentimeter gross, genau richtig für die kleine Tür."
        , easyWords = [ "ja", "das", "glas", "half", "alles", "fiel", "sie", "sass", "leise", "als", "elfe", "da" ]
        , challengeWords = [ "eilige", "elfe", "liesel", "sah", "seidige", "igel", "kegel", "segel", "alles", "gefiel", "liesel" ]
        , madWords = [ "haialiissaadaas", "glaasjadaasglaas", "halfaliissass", "alselfedaaaha", "siiliiflaise", "ailighfiilsii" ]
        , chooseTitle = "Wie gross soll der Schluck sein?"
        , chooseHint = "j / k zum Wählen · Enter zum Trinken"
        , easyInfo = "Kurze Wörter."
        , challengeInfo = "Längere Wörter."
        , madInfo = "Sehr lange, erfundene Wörter."
        , fromMemory = "Lies zuerst das Wort. Sobald du tippst, verschwindet es, und eine Pause mitten im Wort fängt es von vorne an. Zwischen den Wörtern hast du alle Zeit der Welt."
        , tooSlow = "Zu langsam! Das Wort fängt von vorne an."
        , startedOver = "Neu angefangen"
        , sips = "Schlucke"
        , newKeys = "Neue Tasten: e und i. Die Mittelfinger greifen nach oben."
        , bottle = "TRINK MICH"
        }
    , curiouser =
        { intro1 = "Bald fiel Alices Blick auf ein Glaskästchen unter dem Tisch. Darin lag ein winziger Kuchen, auf dem mit Korinthen wunderschön ISS MICH geschrieben stand."
        , intro2 = "Gut, ich esse ihn! Wenn ich davon grösser werde, komme ich an den Schlüssel."
        , intro3 = "Doch als sie den Deckel hob, hüpften die Korinthen-Buchstaben vom Kuchen und verteilten sich über die Krümel auf dem Tisch!"
        , intro4 = "Zerdrück jede Korinthe, bevor sie wegrollt: Bring den Cursor auf sie. Helix hat Sprünge, die mit g anfangen. Die werden dir helfen."
        , finale1 = "Alice ass den Kuchen bis zum letzten Krümel. «Immer merkwürdiger!», rief sie. Sie zog sich aus wie das grösste Fernrohr, das es je gab."
        , finale2 = "Lebt wohl, Füsse!"
        , easyLetters = "ISS MICH UND WACHSE"
        , challengeLetters = "ICH GEHE AUF WIE EIN FERNROHR"
        , madLetters = "LEBT WOHL FÜSSE WER ZIEHT EUCH SCHUHE AN"
        , cake = "ISS MICH"
        , chooseTitle = "Wie schnell sind die Korinthen?"
        , chooseHint = "j / k zum Wählen · Enter zum Starten"
        , easyInfo = "Langsame Korinthen, eine nach der anderen."
        , challengeInfo = "Schnelle Korinthen."
        , madInfo = "Sehr schnelle Korinthen, am Schluss zwei auf einmal."
        , lineEnds = "Eine Korinthe am Ende deiner Zeile! g und dann l springt ans Zeilenende, g und dann h an den Anfang."
        , fileEnds = "Eine Korinthe auf der ersten oder letzten Zeile! g und dann g springt zur allerersten Zeile, g und dann e zur letzten."
        , realLines = "Lange Zeilen gehen auf weiteren Reihen weiter (↪). j geht eine Reihe weiter; g und dann j springt zur nächsten echten Zeile, g und dann k zur vorherigen."
        , lastChange = "Freche Korinthen sind scheu: Sobald du auf ihre Zeile oder Spalte kommst, rennen sie dorthin zurück, wo du die letzte zerdrückt hast. g und dann . springt zu deiner letzten Änderung."
        , allCheeky = "Jetzt sind alle Korinthen frech! Komm auf ihre Zeile oder Spalte, dann fang sie mit g und dann ."
        , cheshireAppears = "Da erschien langsam ein Grinsen in der Luft über dem Tisch, und danach der Rest einer Katze."
        , cheshireSays = "Jede Korinthe mit gw erwischt, und kein einziger Fehler! Wir sind hier alle verrückt. Du musst es auch sein, sonst wärst du nicht hergekommen."
        , newTrick = \keys -> "Neuer Trick: " ++ keys
        , escaped = "Eine Korinthe ist weggerollt!"
        , currants = "Korinthen"
        , squashed = "Zerdrückt"
        }
    , pool =
        { intro1 = "Alice hatte so viel geweint, dass ihre Tränen einen Teich bildeten. Jetzt schwimmt ihr Gedicht darin, und der Teich spritzt immer neue Wörter hinein."
        , intro2 = "Mal sehen, ob ich noch alles weiss, was ich früher wusste. Wie putzt das kleine..."
        , intro3 = "Hier ist die grosse Idee von Helix: erst auswählen, dann handeln. Drücke w und Helix wählt das nächste Wort aus. Drücke d und die Auswahl wird gelöscht."
        , intro4 = "Räum das Gedicht auf: Lösche jedes Wort, das nicht hineingehört. Das Falsche gelöscht? u macht es rückgängig."
        , finale = "Das Gedicht ist sauber und der Teich wird still. Eine Maus schwimmt heran, um alle ans Ufer zu führen."
        , mouse = "Setzt euch alle hin und hört mir zu! Ich mache euch gleich trocken genug!"
        , easyPoem =
            lines
                [ "Wie putzt das kleine SCHWIMM Krokodil"
                , "den Schwanz so SCHNELL blank und fein,"
                , "tränen tränen tränen tränen tränen"
                , "tränen tränen tränen tränen tränen"
                , "und giesst das ANS Wasser aus dem Nil"
                , "auf jede Schuppe ein! UFER"
                ]
        , challengePoem =
            lines
                [ "Wie putzt das PASS kleine Krokodil"
                , "den Schwanz so blank und fein, AUF"
                , "tränen tränen tränen tränen tränen"
                , "tränen tränen tränen tränen tränen"
                , "tränen tränen tränen tränen tränen"
                , "und giesst das Wasser aus dem Nil"
                , "auf jede Schuppe ein!"
                ]
        , madPoem =
            lines
                [ "Wie putzt das kleine fleissige Krokodil"
                , "den hellen Schwanz so blank und fein,"
                , "sie sammelt Honig jeden Tag"
                , "aus jeder offnen Blüte,"
                , "und giesst das süsse Wasser aus dem Nil"
                , "auf jede kleine Schuppe ein!"
                ]
        , cleanPoem =
            lines
                [ "Wie putzt das kleine Krokodil"
                , "den Schwanz so blank und fein,"
                , "und giesst das Wasser aus dem Nil"
                , "auf jede Schuppe ein!"
                ]
        , tideWords = [ "EINE", "MAUS", "KOMMT", "UND", "HILFT", "DIR" ]
        , easyBest = 19
        , madBest = 21
        , ourBest = "Unser Rekord"
        , chooseTitle = "Wie wild ist der Teich?"
        , chooseHint = "j / k zum Wählen · Enter zum Schwimmen"
        , easyInfo = "Fremde Wörter in Grossbuchstaben."
        , challengeInfo = "Der Teich spritzt immer neue Wörter hinein."
        , madInfo = "Manche Wörter passen fast. Lies genau!"
        , hintFind = "Finde ein Wort, das nicht hineingehört. Geh mit h j k l, oder drücke w: Es wählt das nächste Wort aus."
        , hintDelete = "Ausgewählt! d löscht die Auswahl."
        , hintLines =
            \count ->
                if count == 1 then
                    "Diese ganze Zeile gehört nicht hinein. x wählt die Zeile aus."

                else
                    "Diese " ++ String.fromInt count ++ " Zeilen gehören nicht hinein. " ++ String.fromInt count ++ "x wählt " ++ String.fromInt count ++ " Zeilen auf einmal aus."
        , hintAnyLine = "x wählt eine ganze Zeile aus. Eine Zahl davor wählt mehr Zeilen: 2x."
        , hintUndo = "Ein Wort aus dem Gedicht fehlt. u macht die letzte Änderung rückgängig."
        , hintCount = "Eine Zahl davor wiederholt eine Taste: 3w wählt das dritte Wort voraus."
        , hintLineStart = "Eine Abkürzung: g und dann h springt an den Zeilenanfang."
        , wordsLeft = \count -> "Noch zu löschen: " ++ String.fromInt count
        , goal = "So soll das Gedicht lauten:"
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
        , challengeStart =
            lines
                [ "\"Was meinst du dazu?\", fragte die Raupe streng."
                , "\"Erkläre !\""
                , "\"Ich kann mich nicht erklären, fürchte ich\", sagte"
                , "\"Bewahre die Mütze\", sagte die Raupe."
                ]
        , challengeAnswer =
            lines
                [ "\"Was meinst du damit?\", fragte die Raupe streng."
                , "\"Erkläre dich!\""
                , "\"Ich kann mich nicht erklären, fürchte ich\", sagte Alice"
                , "\"Bewahre die Fassung\", sagte die Raupe."
                ]
        , madStart =
            lines
                [ "\"Du bist jung, Vater William\", sprach der alte Mann,"
                , "\"und dein Haar ist weiss und dünn;"
                , "doch du sitzt auf dem Hut, so oft man nur kann."
                , "Hat das in deinem Alter noch Sinn?\""
                , "Und die Raupe rauchte die lange Pfeife."
                , "\"In der Jugend\", sprach Vater William zum"
                , "\"da hatte ich Angst um mein Ohr;"
                , "doch jetzt weiss ich, ich habe keins schon,"
                , "drum steh ich und steh auf der"
                ]
        , madAnswer =
            lines
                [ "\"Du bist alt, Vater William\", sprach der junge Mann,"
                , "\"und dein Haar ist schon weiss und dünn;"
                , "doch du stehst auf dem Kopf, so oft man nur kann."
                , "Hat das in deinem Alter noch Sinn?\""
                , "\"In der Jugend\", sprach Vater William zum Sohn,"
                , "\"da hatte ich Angst um mein Hirn;"
                , "doch jetzt weiss ich sicher, ich habe keins schon,"
                , "drum steh ich und steh auf der Stirn.\""
                ]
        , madBest = 100
        , chooseTitle = "Wie mürrisch ist die Raupe?"
        , chooseHint = "j / k zum Wählen · Enter zum Starten"
        , easyInfo = "Füll die Lücken."
        , challengeInfo = "Lücken, falsche Wörter und Zeilenenden."
        , madInfo = "Alice sagt ein ganzes Gedicht auf, alles falsch."
        , hintFind = "Vergleiche mit dem Ziel. Fehlt ein Wort: i fügt vor dem Cursor ein. Ein falsches Wort: Wähle es mit w aus, dann c. Eine Zeile, die nicht hineingehört: x, dann d."
        , hintChange = "Ausgewählt! c löscht die Auswahl und beginnt mit dem Einfügen: Tippe das richtige Wort."
        , hintAppend = "Am Ende einer Zeile fügt a hinter dem Cursor ein (mit gl kommst du hin)."
        , hintTyping = "Tippe, dann mit Esc zurück zu NOR."
        , ourBest = "Unser Rekord"
        , goal = "Was die Raupe wirklich sagt:"
        , tip = "a fügt hinter dem Cursor ein. c löscht eine Auswahl und beginnt mit dem Einfügen."
        , escTip = "Wenn Esc im Browser nicht geht, funktioniert auch Strg+[."
        }
    }
