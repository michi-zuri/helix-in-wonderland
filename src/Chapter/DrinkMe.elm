module Chapter.DrinkMe exposing (Hint(..), Model, Msg(..), alicePixelSize, init, ourBest, timeLimit, update, view)

{-| Chapter 4, "Drink Me": changing whole words, quickly (#74). Each word is
one sip from the bottle, and Alice gets smaller; the golden key she put back
on the glass table stays where it is, until it is far out of her reach.

The lines start as stars (`***`), one line a word, with the word to write
next to each line. `e` selects the next stars, `c` changes them: the word,
then `j k` back to Normal mode. A change has to be done in time,
`timeLimit`: 300 ms a letter of the word plus 600 ms for `j k`, twice that
on Easy (#140); a change too
slow is undone. When the same word comes again, `e .` repeats the change.
`h j k l` and `i` work too, but a dialog says once that `e c .` are the
quicker way. A wrong word that was escaped in time just stays: the player
goes back with `k` and changes it again.

The difficulty picks the words: short, longer, or very long made-up ones
(`Texts.DrinkMeTexts`).

Time comes only from the `Frame` messages, so tests can type at any pace.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
import Editor.Core as Core exposing (Buffer, Command(..), Mode(..))
import Editor.Session as Session exposing (Session)
import Editor.View
import Html exposing (Html)
import Html.Attributes as A
import Html.Lazy
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Texts exposing (Texts)
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "04_drink_me.txt"


placeholder : String
placeholder =
    "***"



-- TUNING


{-| How long a change may take, from `c` (or `i`) to the `k` of `j k`, in
ms: 300 a letter of the line's word, and 600 for `j k`. Easy allows twice
as much (#140).
-}
timeLimit : Difficulty -> String -> Float
timeLimit difficulty word =
    let
        normal =
            300 * toFloat (String.length word) + 600
    in
    if difficulty == Easy then
        2 * normal

    else
        normal


{-| Alice's size in screen pixels per sprite pixel, before the first sip and
after the last: twice the little door's height at first (as in chapter 3),
ten inches at the end, two thirds of the fifteen-inch door.
-}
tallest : Float
tallest =
    2.75


smallest : Float
smallest =
    0.92


walkMilliseconds : Float
walkMilliseconds =
    1800



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , work : Maybe Work
    , sip : Maybe Sip
    , hintsGiven : List Hint
    , tooSlow : Int
    , wrongWords : Int
    , alicePixelSize : Float
    , walk : Maybe Float
    , clock : Float
    }


{-| The words in the language the player started in, and the lines.
-}
type alias Work =
    { words : List String
    , session : Session
    }


{-| A change in progress: the lines before it (put back if it takes too
long), how long it has taken so far and may take.
-}
type alias Sip =
    { before : Buffer
    , elapsed : Float
    , limit : Float
    }


{-| The hints, each told once in a dialog, the first time it helps.
-}
type Hint
    = HowTo
    | OtherKeys
    | Repeat
    | TooSlow
    | WrongWord


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.drinkMe >> .intro1))
            [ Dialog.say Narrator (.drinkMe >> .intro2)
            , Dialog.say Narrator (.drinkMe >> .intro3)
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , work = Nothing
    , sip = Nothing
    , hintsGiven = []
    , tooSlow = 0
    , wrongWords = 0
    , alicePixelSize = tallest
    , walk = Nothing
    , clock = 0
    }


wordsFor : Texts -> Difficulty -> List String
wordsFor texts difficulty =
    case difficulty of
        Easy ->
            texts.drinkMe.easyWords

        Challenge ->
            texts.drinkMe.challengeWords

        Mad ->
            texts.drinkMe.madWords


{-| The cursor starts on the first stars, so `e` selects them (#122).
-}
startWork : List String -> Work
startWork words =
    { words = words
    , session = Session.start (String.join "\n" (List.map (always placeholder) words))
    }


{-| The words follow the language setting until a difficulty is picked.
-}
currentWork : Texts -> Model -> Work
currentWork texts model =
    case model.work of
        Just work ->
            work

        Nothing ->
            startWork (wordsFor texts (Maybe.withDefault Easy model.difficulty))


isInsert : Session -> Bool
isInsert session =
    Core.mode (Session.buffer session) == Insert


{-| The lines as they are now.
-}
lines : Work -> List String
lines work =
    Session.exactLines (Core.toString (Session.buffer work.session))


{-| For each word, does its line read exactly like it?
-}
wordsRight : Work -> List Bool
wordsRight work =
    List.map2 (==) work.words (lines work ++ List.repeat (List.length work.words) "")


isDone : Work -> Bool
isDone work =
    not (isInsert work.session) && lines work == work.words


{-| The word of a line (1-based, as the status line counts), if it has one.
-}
wordOfLine : Work -> Int -> Maybe String
wordOfLine work line =
    List.drop (line - 1) work.words |> List.head


cursorLine : Work -> Int
cursorLine work =
    Tuple.first (Core.cursorLineColumn (Session.buffer work.session))


{-| The fewest keystrokes we found, and play in a test: `e c`, the word,
`j k` for every word, and only `e .` for a word straight after the same
word.
-}
ourBest : List String -> Int
ourBest words =
    List.map2
        (\previous word ->
            if previous == Just word then
                2

            else
                4 + String.length word
        )
        (Nothing :: List.map Just words)
        words
        |> List.sum



-- UPDATE


type Msg
    = KeyPressed String
    | Frame Float


update : Context -> Msg -> Model -> ( Model, List Effect )
update context msg model =
    case msg of
        KeyPressed key ->
            case Story.hear context.texts key model.story of
                Story.NotTalking ->
                    if Story.isPlaying model.story && model.walk == Nothing then
                        case model.difficulty of
                            Just _ ->
                                edit context.texts key model

                            Nothing ->
                                choose context.texts key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    ( { model | story = story }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story, choice = DifficultyChoice.start model.clock }, [ ShowToast (.drinkMe >> .newKeys) ] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score context.texts model) ] )

        Frame milliseconds ->
            frame context.texts milliseconds model


choose : Texts -> String -> Model -> ( Model, List Effect )
choose texts key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            ( { model | difficulty = Just difficulty, work = Just (startWork (wordsFor texts difficulty)) }
                |> tell HowTo (.drinkMe >> .hintHowTo)
            , [ PlaySound PickupSound ]
            )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| What the player knows by now: `h j k l` from chapter 2, `i` and `c`
from chapter 3, this chapter's `e` and `.`, and `gg` to get back to the
first word (#122), with a count to any line. Counts work but aren't needed. Enter and Backspace work
in INSERT as anywhere: a deleted line break can be typed back.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    List.member command [ MoveLeft, MoveRight, MoveDown, MoveUp, SelectNextWordEnd, Change, InsertBefore, ExitInsert, RepeatLastChange, Backspace, CollapseSelection, GotoFileStart ]
        || (case command of
                TypeChar _ ->
                    True

                -- A count before gg goes to that line (`2gg`); it works, but
                -- isn't mentioned.
                GotoLineNumber _ ->
                    True

                _ ->
                    False
           )


edit : Texts -> String -> Model -> ( Model, List Effect )
edit texts key model =
    let
        work =
            currentWork texts model
    in
    case Session.press isUnlocked key work.session of
        Session.Ran ran ->
            notice ran.command ran.before { model | work = Just { work | session = ran.session } }

        Session.Waiting session ->
            ( { model | work = Just { work | session = session } }, [] )

        Session.Locked session ->
            ( dropCount work session model, [ ShowToast .notYet ] )

        Session.Ignored session ->
            ( dropCount work session model, [] )


notice : Command -> Buffer -> Model -> ( Model, List Effect )
notice command before model =
    case ( command, model.work ) of
        ( Change, Just work ) ->
            ( startSip work before model, [ PlaySound PickupSound ] )

        ( InsertBefore, Just work ) ->
            ( startSip work before model |> tell OtherKeys (.drinkMe >> .hintOtherKeys), [ PlaySound PickupSound ] )

        ( ExitInsert, Just work ) ->
            sipped work { model | sip = Nothing }

        ( RepeatLastChange, Just work ) ->
            sipped work model

        ( TypeChar _, _ ) ->
            ( model, [ PlaySound TickSound ] )

        ( MoveLeft, _ ) ->
            ( tell OtherKeys (.drinkMe >> .hintOtherKeys) model, [] )

        ( MoveRight, _ ) ->
            ( tell OtherKeys (.drinkMe >> .hintOtherKeys) model, [] )

        ( MoveDown, _ ) ->
            ( tell OtherKeys (.drinkMe >> .hintOtherKeys) model, [] )

        ( MoveUp, _ ) ->
            -- On the first line k can't go higher: every time, gg is the way (#142).
            if Tuple.first (Core.cursorLineColumn before) == 1 then
                ( tellAgain (.drinkMe >> .firstLineUp) model, [ PlaySound BumpSound ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )


startSip : Work -> Buffer -> Model -> Model
startSip work before model =
    let
        longest =
            longestWord work
    in
    { model
        | sip =
            Just
                { before = before
                , elapsed = 0
                , limit =
                    wordOfLine work (cursorLine work)
                        |> Maybe.withDefault (String.repeat longest "*")
                        |> timeLimit (Maybe.withDefault Easy model.difficulty)
                }
    }


{-| A change is done (`j k`, or `.`): is the line its word now? All lines
right: Alice walks to the door.
-}
sipped : Work -> Model -> ( Model, List Effect )
sipped work model =
    let
        line =
            cursorLine work

        typed =
            List.drop (line - 1) (lines work) |> List.head
    in
    if isDone work then
        ( { model | walk = Just 0 }, [ PlaySound PickupSound ] )

    else
        case wordOfLine work line of
            Just word ->
                if typed == Just word then
                    let
                        next =
                            wordOfLine work (line + 1)

                        nextTyped =
                            List.drop line (lines work) |> List.head
                    in
                    ( if next == Just word && nextTyped /= next then
                        tell Repeat (.drinkMe >> .hintRepeat) model

                      else
                        model
                    , [ PlaySound CorrectSound ]
                    )

                else
                    let
                        counted =
                            { model | wrongWords = model.wrongWords + 1 }
                    in
                    if line == 1 then
                        -- k can't get back to the first word: gg can, said every time (#142).
                        ( tellAgain (.drinkMe >> .firstLineWrong) counted, [ PlaySound ErrorSound ] )

                    else if List.member WrongWord model.hintsGiven then
                        ( counted, [ PlaySound ErrorSound, ShowToast (.drinkMe >> .wrongWordShort) ] )

                    else
                        ( tell WrongWord (.drinkMe >> .wrongWord) counted, [ PlaySound ErrorSound ] )

            Nothing ->
                ( model, [] )


{-| A key that ran nothing drops a count typed before it. Otherwise the
model stays as it was, so the words keep following the language setting.
-}
dropCount : Work -> Session -> Model -> Model
dropCount work session model =
    if not (Session.isWaiting work.session) then
        model

    else
        { model | work = Just { work | session = session } }


frame : Texts -> Float -> Model -> ( Model, List Effect )
frame texts milliseconds model =
    let
        work =
            currentWork texts model

        right =
            List.length (List.filter identity (wordsRight work))

        targetSize =
            tallest - (tallest - smallest) * toFloat right / toFloat (max 1 (List.length work.words))

        advanced =
            { model
                | clock = model.clock + milliseconds
                , story = Story.tick milliseconds model.story
                , alicePixelSize = model.alicePixelSize + (targetSize - model.alicePixelSize) * min 1 (milliseconds * 0.004)
                , walk = Maybe.map (\progress -> min 1 (progress + milliseconds / walkMilliseconds)) model.walk
            }
    in
    case ( advanced.walk, advanced.sip ) of
        ( Just progress, _ ) ->
            if progress >= 1 && Story.isPlaying advanced.story then
                ( { advanced
                    | story =
                        Story.conclude (Dialog.say Narrator (.drinkMe >> .finale1))
                            [ Dialog.say Narrator (.drinkMe >> .finale2) ]
                  }
                , []
                )

            else
                ( advanced, [] )

        ( Nothing, Just sip ) ->
            -- The time runs only while the player can type: never during a dialog.
            if Story.isPlaying advanced.story then
                let
                    elapsed =
                        sip.elapsed + milliseconds
                in
                if elapsed > sip.limit then
                    tooSlow work sip advanced

                else
                    ( { advanced | sip = Just { sip | elapsed = elapsed } }, [] )

            else
                ( advanced, [] )

        ( Nothing, Nothing ) ->
            ( advanced, [] )


{-| The change took too long: it is undone, the stars are back.
-}
tooSlow : Work -> Sip -> Model -> ( Model, List Effect )
tooSlow work sip model =
    let
        undone =
            { model
                | work = Just { work | session = Session.withBuffer sip.before work.session }
                , sip = Nothing
                , tooSlow = model.tooSlow + 1
            }
    in
    if List.member TooSlow model.hintsGiven then
        ( undone, [ PlaySound BumpSound, ShowToast (.drinkMe >> .tooSlowShort) ] )

    else
        ( tell TooSlow (.drinkMe >> .tooSlow) undone, [ PlaySound BumpSound ] )


{-| Tell a hint in a dialog, however often it was told before.
-}
tellAgain : (Texts -> String) -> Model -> Model
tellAgain text model =
    { model | story = Story.tell (Dialog.say Narrator text) [] model.story }


{-| Tell a hint in a dialog, unless it was told before.
-}
tell : Hint -> (Texts -> String) -> Model -> Model
tell hint text model =
    if List.member hint model.hintsGiven then
        model

    else
        { model
            | hintsGiven = hint :: model.hintsGiven
            , story = Story.tell (Dialog.say Narrator text) [] model.story
        }


{-| The difficulty sets the most tarts there are (easy 1, challenge 2,
mad 3); changes too slow and wrong words can cost some of them.
-}
score : Texts -> Model -> Contract.Score
score texts model =
    let
        difficulty =
            Maybe.withDefault Easy model.difficulty

        work =
            currentWork texts model

        mistakes =
            model.tooSlow + model.wrongWords

        forMistakes =
            if mistakes <= 1 then
                3

            else if mistakes <= 4 then
                2

            else
                1
    in
    { tarts = min (Difficulty.tarts difficulty) forMistakes
    , stats =
        [ { label = .results >> .difficulty, value = \t -> Difficulty.name t difficulty }
        , { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes work.session) }
        , { label = .drinkMe >> .ourBest, value = \_ -> String.fromInt (ourBest work.words) }
        , { label = .drinkMe >> .tooSlowCount, value = \_ -> String.fromInt model.tooSlow }
        , { label = .drinkMe >> .wrongWords, value = \_ -> String.fromInt model.wrongWords }
        ]
    }


{-| Alice's size now, for tests: she shrinks with every word that is right.
-}
alicePixelSize : Model -> Float
alicePixelSize model =
    model.alicePixelSize



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    Story.instructions model.story (scene context model)


{-| Everything the chapter shows. There is no panel: hints come in dialogs,
and the words stand next to their lines, so everything fits on one screen.
The status line is Helix's own.
-}
scene : Context -> Model -> Contract.View msg
scene context model =
    let
        work =
            currentWork context.texts model

        buffer =
            Session.buffer work.session
    in
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ viewHall context.texts model
            , viewKeyMenu context.texts work.session
            , Html.div [ A.class "editor-with-words" ]
                [ -- As wide as the longest word and a little more, so the
                  -- words stand next to their lines and don't move while typing.
                  Html.div
                    [ A.class "lines-box"
                    , A.style "width" (String.fromInt (longestWord work + 9) ++ "ch")
                    ]
                    [ Html.Lazy.lazy viewEditor work.session ]
                , if model.difficulty == Nothing then
                    Html.text ""

                  else
                    viewWords work model
                ]
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.drinkMe.chooseTitle
                    , hint = context.texts.drinkMe.chooseHint
                    , info = difficultyInfo context.texts
                    }
                    model.choice

              else
                Html.text ""
            , Story.view context.texts model.story
            ]
    , panel = Html.text ""
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys work.session)
        , position = Editor.View.position buffer
        }
    }


longestWord : Work -> Int
longestWord work =
    List.map String.length work.words |> List.maximum |> Maybe.withDefault 0


viewEditor : Session -> Html msg
viewEditor session =
    Editor.View.view (Session.buffer session)


{-| The words, each next to its line, in a column as tall as the editor's
lines. A word turns green
once its line reads right; while a change is made, a bar under its word
shows the time left.
-}
viewWords : Work -> Model -> Html msg
viewWords work model =
    let
        line =
            cursorLine work

        timeBar index =
            case model.sip of
                Just sip ->
                    if index + 1 == line then
                        [ Html.span
                            [ A.class "time-left"
                            , A.style "width" (String.fromFloat (100 * max 0 (1 - sip.elapsed / sip.limit)) ++ "%")
                            ]
                            []
                        ]

                    else
                        []

                Nothing ->
                    []
    in
    Html.div [ A.class "line-words" ]
        (List.map3
            (\index word right ->
                Html.div [ A.classList [ ( "line-word", True ), ( "right", right ) ] ]
                    (Html.text word :: timeBar index)
            )
            (List.range 0 (List.length work.words - 1))
            work.words
            (wordsRight work)
        )


{-| While a `j` in INSERT waits, the little menu of what comes next (#110).
-}
viewKeyMenu : Texts -> Session -> Html msg
viewKeyMenu texts session =
    if Session.isAfterJ session then
        Editor.View.keyMenu [ ( "k", texts.keyMenu.normal ), ( "j", texts.keyMenu.typeJ ) ]

    else
        Html.text ""


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.drinkMe.easyInfo

        Challenge ->
            texts.drinkMe.challengeInfo

        Mad ->
            texts.drinkMe.madInfo


{-| The hall seen from the side: the glass table with the bottle and the
golden key on it, the little door behind its curtain, and Alice, who gets
smaller while the table and the key stay as they are. At first she is
twice as tall as the door and reaches over the table top; at the end the
key lies far above her head.
-}
viewHall : Texts -> Model -> Html msg
viewHall texts model =
    let
        label =
            texts.drinkMe.bottle

        -- The label font is 7 px per character at 11 px.
        labelWidth =
            toFloat (String.length label * 7 + 8)

        pixelSize =
            model.alicePixelSize

        walk =
            Maybe.withDefault 0 model.walk

        aliceCentre =
            92 + (296 - 92) * walk

        walking =
            model.walk /= Nothing && walk < 1 && modBy 2 (floor (model.clock * Sprites.walkingFramesPerSecond / 1000)) == 1

        floor_ =
            72
    in
    Svg.svg [ SA.viewBox "0 0 320 80", SA.class "banner hall-banner" ]
        [ rect 0 0 320 floor_ "#4a3060"
        , Svg.g [] (List.map (\x -> rect (toFloat x) 0 1 floor_ "#432a58") (List.range 0 13 |> List.map ((*) 24)))
        , rect 0 (floor_ - 6) 320 6 "#5a3d73"
        , rect 0 floor_ 320 8 "#33223f"

        -- the curtain and the little door, fifteen inches high
        , rect 282 (floor_ - 30) 30 30 "#b8435a"
        , rect 288 (floor_ - 22) 16 22 "#7a5634"
        , rect 300 (floor_ - 11) 2 2 "#efba5d"

        -- the glass table, with three glass legs
        , translucent 130 40 84 3 "#bfe6f0" 0.35
        , rect 130 40 84 1 "#bfe6f0"
        , translucent 134 43 2 (floor_ - 43) "#bfe6f0" 0.45
        , translucent 171 43 2 (floor_ - 43) "#bfe6f0" 0.45
        , translucent 208 43 2 (floor_ - 43) "#bfe6f0" 0.45

        -- the golden key, lying on the table where Alice left it
        , Sprite.viewAt { x = 144, y = 36.4, pixelSize = 1.2 } Sprites.goldenKey

        -- the bottle with its paper label on a string
        , rect 189 18 6 3 "#8a5a3c"
        , translucent 190 21 4 4 "#a4a0e8" 0.7
        , translucent 186 25 12 15 "#a4a0e8" 0.7
        , rect 187 29 10 6 "#f4ead2"
        , rect 192 13 1 6 "#dbbfef"
        , rect (192 - labelWidth / 2) 1 labelWidth 12 "#f4ead2"
        , rect (192 - labelWidth / 2) 12 labelWidth 1 "#120a18"
        , Svg.text_ [ SA.x "192", SA.y "10", SA.class "bottle-label", SA.textAnchor "middle" ] [ Svg.text label ]
        , Sprite.viewAt
            { x = aliceCentre - 6 * pixelSize
            , y = floor_ - 16 * pixelSize
            , pixelSize = pixelSize
            }
            (if walking then
                Sprites.aliceStepping

             else
                Sprites.alice
            )
        ]



-- SVG HELPERS


rect : Float -> Float -> Float -> Float -> String -> Svg msg
rect x y w h color =
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , SA.fill color
        ]
        []


translucent : Float -> Float -> Float -> Float -> String -> Float -> Svg msg
translucent x y w h color opacity =
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , SA.fill color
        , SA.fillOpacity (String.fromFloat opacity)
        ]
        []
