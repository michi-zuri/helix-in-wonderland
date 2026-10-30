module Chapter.Pool exposing (Model, Msg(..), init, update, view)

{-| Chapter 4, "The Pool of Tears": the big Helix idea, select first and
then act. The player cleans a poem with `w` (select the next word), `x`
(select the line), `d` (delete) and `u` (undo), with counts like `3w` and
`2x` (see `Editor.Session`). `e` and `b` wait for a later chapter, and
Insert-mode keys for chapter 5.

Three difficulties (see `Texts.PoolTexts` for the poems):

  - easy: odd words in capitals, and lines of tears;
  - challenge: the pool keeps splashing new words in, one every few
    seconds, until the tide is over;
  - mad: the odd words almost belong; they come from the poem Alice mixes
    the crocodile up with.

A deletion is judged by one rule for all three: it is wrong if the clean
poem no longer fits inside the text (`cleanFits`), right if it removed
words and the clean poem still fits. Hints show one at a time, when they
are useful (`hints`); on mad none of them give away which words are odd.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
import Editor.Core as Core exposing (Buffer, Command(..))
import Editor.Session as Session exposing (Session)
import Editor.View
import Html exposing (Html)
import Html.Attributes as A
import Html.Lazy
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Random
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy
import Texts exposing (PoolTexts, Texts)
import Ui.Checklist as Checklist
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "04_pool_of_tears.txt"



-- TUNING


{-| On challenge, a new word splashes in this often, in milliseconds.
-}
tideMilliseconds : Float
tideMilliseconds =
    6000


{-| When the poem is clean but the tide isn't over, the next word comes
sooner, so nobody waits around.
-}
calmMilliseconds : Float
calmMilliseconds =
    1500


{-| After this many `w` presses in a row, a tip suggests a count.
-}
wordsBeforeCountTip : Int
wordsBeforeCountTip =
    3



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , work : Maybe Work
    , seed : Random.Seed
    , tide : Tide
    , wrongDeletions : Int
    , wordsInARow : Int
    , usedCount : Bool
    , clock : Float
    }


{-| The poem being cleaned, fixed when a difficulty is picked (until then
the text follows the language setting).
-}
type alias Work =
    { puzzle : Puzzle
    , session : Session
    }


type alias Puzzle =
    { poem : String
    , cleanPoem : String
    , tideWords : List String
    , ourBest : Maybe Int
    }


{-| How many of the tide's words have splashed in, and how long until the
next one.
-}
type alias Tide =
    { splashed : Int
    , untilNext : Float
    }


{-| The seed decides where the tide splashes its words.
-}
init : Random.Seed -> Model
init seed =
    { story =
        Story.begin (Dialog.say Narrator (.pool >> .intro1))
            [ Dialog.say Alice (.pool >> .intro2)
            , Dialog.say Narrator (.pool >> .intro3)
            , Dialog.say Narrator (.pool >> .intro4)
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , work = Nothing
    , seed = seed
    , tide = { splashed = 0, untilNext = tideMilliseconds }
    , wrongDeletions = 0
    , wordsInARow = 0
    , usedCount = False
    , clock = 0
    }


puzzleFor : PoolTexts -> Difficulty -> Puzzle
puzzleFor texts difficulty =
    case difficulty of
        Easy ->
            { poem = texts.easyPoem, cleanPoem = texts.cleanPoem, tideWords = [], ourBest = Just texts.easyBest }

        Challenge ->
            { poem = texts.challengePoem, cleanPoem = texts.cleanPoem, tideWords = texts.tideWords, ourBest = Nothing }

        Mad ->
            { poem = texts.madPoem, cleanPoem = texts.cleanPoem, tideWords = [], ourBest = Just texts.madBest }


currentWork : Texts -> Model -> Work
currentWork texts model =
    case model.work of
        Just work ->
            work

        Nothing ->
            let
                puzzle =
                    puzzleFor texts.pool (Maybe.withDefault Easy model.difficulty)
            in
            { puzzle = puzzle, session = Session.start puzzle.poem }



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
                    if Story.isPlaying model.story then
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
                    ( { model | story = story, choice = DifficultyChoice.start model.clock }, [] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score context.texts model) ] )

        Frame milliseconds ->
            { model | clock = model.clock + milliseconds, story = Story.tick milliseconds model.story }
                |> flow milliseconds


choose : Texts -> String -> Model -> ( Model, List Effect )
choose texts key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            let
                puzzle =
                    puzzleFor texts.pool difficulty
            in
            ( { model | difficulty = Just difficulty, work = Just { puzzle = puzzle, session = Session.start puzzle.poem } }
            , [ PlaySound PickupSound ]
            )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| Moving, `w`, `x`, `d`, `u` and counts. `e` and `b` come in a later
chapter, inserting text in chapter 5.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    not (List.member command [ SelectNextWordEnd, SelectPreviousWordStart, InsertBefore, InsertAfter, Change ])


edit : Texts -> String -> Model -> ( Model, List Effect )
edit texts key model =
    let
        work =
            currentWork texts model
    in
    case Session.press isUnlocked key work.session of
        Session.Ran ran ->
            { model
                | work = Just { work | session = ran.session }
                , usedCount = model.usedCount || ran.count > 1
                , wordsInARow =
                    if ran.command == SelectNextWordStart && ran.count == 1 then
                        model.wordsInARow + 1

                    else
                        0
            }
                |> judge work.puzzle ran
                |> Tuple.mapSecond ((::) (FlashKey key True))
                |> finishIfClean work.puzzle ran.session

        Session.Counting session ->
            ( { model | work = Just { work | session = session } }, [ FlashKey key True, PlaySound TickSound ] )

        Session.Locked session ->
            ( keepCount work session model, [ ShowToast .notYet, PlaySound ErrorSound, FlashKey key False ] )

        Session.Ignored session ->
            ( keepCount work session model, [] )


{-| A key that ran nothing drops a count typed before it.
-}
keepCount : Work -> Session -> Model -> Model
keepCount work session model =
    if Session.pendingCount work.session == "" then
        model

    else
        { model | work = Just { work | session = session } }


{-| A deletion is wrong when the clean poem no longer fits inside the
text, right when it removed words and the poem still fits. Deleting only
spaces is neither.
-}
judge : Puzzle -> { a | command : Command, before : Buffer, session : Session } -> Model -> ( Model, List Effect )
judge puzzle ran model =
    if ran.command == Delete then
        let
            after =
                Core.toString (Session.buffer ran.session)

            removedWords =
                List.length (wordsIn (Core.toString ran.before)) > List.length (wordsIn after)
        in
        if cleanFits puzzle.cleanPoem (Core.toString ran.before) && not (cleanFits puzzle.cleanPoem after) then
            ( { model | wrongDeletions = model.wrongDeletions + 1 }
            , [ ShowToast (.pool >> .wrongDeletion), PlaySound ErrorSound ]
            )

        else if removedWords then
            ( model, [ PlaySound CorrectSound ] )

        else
            ( model, [] )

    else
        ( model, [] )


finishIfClean : Puzzle -> Session -> ( Model, List Effect ) -> ( Model, List Effect )
finishIfClean puzzle session ( model, effects ) =
    if Session.matches puzzle.cleanPoem session && model.tide.splashed >= List.length puzzle.tideWords then
        ( { model
            | story =
                Story.conclude (Dialog.say Narrator (.pool >> .finale))
                    [ Dialog.say Mouse (.pool >> .mouse) ]
          }
        , effects ++ [ PlaySound PickupSound ]
        )

    else
        ( model, effects )


{-| Time passing: on challenge, the tide splashes its next word in.
-}
flow : Float -> Model -> ( Model, List Effect )
flow milliseconds model =
    case model.work of
        Just work ->
            let
                tide =
                    model.tide

                waiting =
                    if Session.matches work.puzzle.cleanPoem work.session then
                        min tide.untilNext calmMilliseconds - milliseconds

                    else
                        tide.untilNext - milliseconds
            in
            if Story.isPlaying model.story && tide.splashed < List.length work.puzzle.tideWords then
                if waiting <= 0 then
                    splashNext work { model | tide = { tide | untilNext = tideMilliseconds } }

                else
                    ( { model | tide = { tide | untilNext = waiting } }, [] )

            else
                ( model, [] )

        Nothing ->
            ( model, [] )


{-| The tide's next word lands in front of a random word of a random line.
-}
splashNext : Work -> Model -> ( Model, List Effect )
splashNext work model =
    let
        text =
            Core.toString (Session.buffer work.session)

        lineWords =
            String.lines text
                |> List.map (String.words >> List.length)
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, count ) -> count > 0)

        placeGenerator =
            case lineWords of
                first :: others ->
                    Random.uniform first others
                        |> Random.andThen (\( lineIndex, count ) -> Random.map (\wordIndex -> ( lineIndex, wordIndex )) (Random.int 0 count))

                [] ->
                    Random.constant ( 0, 0 )

        ( ( line, word ), seed ) =
            Random.step placeGenerator model.seed

        tide =
            model.tide
    in
    case List.drop tide.splashed work.puzzle.tideWords |> List.head of
        Just tideWord ->
            ( { model
                | seed = seed
                , tide = { tide | splashed = tide.splashed + 1 }
                , work = Just { work | session = Session.splash { line = line, word = word, text = tideWord } work.session }
              }
            , [ PlaySound PortalSound ]
            )

        Nothing ->
            ( model, [] )



-- READING THE POEM


{-| The words of a text, without punctuation: "scale! SHORE" gives
["scale", "SHORE"].
-}
wordsIn : String -> List String
wordsIn text =
    String.words text
        |> List.map (String.filter (\c -> Char.isAlphaNum c || Char.toCode c > 127))
        |> List.filter (not << String.isEmpty)


{-| Can the clean poem still be made by deleting words from the text? Its
words must all be there, in the same order.
-}
cleanFits : String -> String -> Bool
cleanFits clean text =
    isInside (wordsIn clean) (wordsIn text)


isInside : List String -> List String -> Bool
isInside wanted words =
    case ( wanted, words ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( first :: restWanted, word :: restWords ) ->
            if first == word then
                isInside restWanted restWords

            else
                isInside wanted restWords


{-| How high the water stands, from 0 (the poem is clean) to 1 (as many
words to delete as at the start, or more after splashes).
-}
waterLevel : Puzzle -> Buffer -> Float
waterLevel puzzle buffer =
    let
        atStart =
            max 1 (List.length (wordsIn puzzle.poem) - List.length (wordsIn puzzle.cleanPoem))
    in
    min 1 (toFloat (wordsToDelete puzzle buffer) / toFloat atStart)


wordsToDelete : Puzzle -> Buffer -> Int
wordsToDelete puzzle buffer =
    max 0 (List.length (wordsIn (Core.toString buffer)) - List.length (wordsIn puzzle.cleanPoem))


{-| How many lines from the cursor's line on could go entirely, with the
clean poem still fitting: 0 when the cursor's line belongs to the poem.
-}
oddLinesHere : Puzzle -> Buffer -> Int
oddLinesHere puzzle buffer =
    let
        allLines =
            String.lines (Core.toString buffer)

        ( cursorLine, _ ) =
            Core.cursorLineColumn buffer

        withoutLines first count =
            List.take first allLines ++ List.drop (first + count) allLines

        isOdd count =
            let
                removed =
                    allLines |> List.drop (cursorLine - 1) |> List.take count
            in
            List.length removed
                == count
                && List.all (\line -> not (List.isEmpty (wordsIn line))) removed
                && cleanFits puzzle.cleanPoem (String.join "\n" (withoutLines (cursorLine - 1) count))

        grow count =
            if isOdd (count + 1) then
                grow (count + 1)

            else
                count
    in
    grow 0


hasSelection : Buffer -> Bool
hasSelection buffer =
    let
        ( first, last ) =
            Core.selectionRange buffer
    in
    first /= last && not (List.isEmpty (wordsIn (Core.selectedText buffer)))


{-| The hints to show now, most important first. Only one main hint at a
time, so a child reads it; mad never says which words or lines are odd.
-}
hints : PoolTexts -> Difficulty -> Model -> Puzzle -> Buffer -> List String
hints texts difficulty model puzzle buffer =
    let
        oddLines =
            oddLinesHere puzzle buffer

        main =
            if not (cleanFits puzzle.cleanPoem (Core.toString buffer)) then
                [ texts.hintUndo ]

            else if hasSelection buffer then
                [ texts.hintDelete ]

            else if difficulty /= Mad && oddLines > 0 then
                [ texts.hintLines oddLines ]

            else if difficulty == Mad then
                [ texts.hintFind, texts.hintAnyLine ]

            else
                [ texts.hintFind ]

        countTip =
            if model.wordsInARow >= wordsBeforeCountTip && not model.usedCount then
                [ texts.hintCount ]

            else
                []
    in
    main ++ countTip


{-| The difficulty sets the most tarts there are (easy 1, challenge 2,
mad 3); wrong deletions can cost some of them.
-}
score : Texts -> Model -> Contract.Score
score texts model =
    let
        work =
            currentWork texts model

        difficulty =
            Maybe.withDefault Easy model.difficulty

        forMistakes =
            if model.wrongDeletions == 0 then
                3

            else if model.wrongDeletions <= 2 then
                2

            else
                1
    in
    { tarts = min (Difficulty.tarts difficulty) forMistakes
    , stats =
        List.concat
            [ [ { label = .results >> .difficulty, value = \t -> Difficulty.name t difficulty }
              , { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes work.session) }
              ]
            , case work.puzzle.ourBest of
                Just best ->
                    [ { label = .pool >> .ourBest, value = \_ -> String.fromInt best } ]

                Nothing ->
                    []
            , [ { label = .results >> .wrongDeletions, value = \_ -> String.fromInt model.wrongDeletions } ]
            ]
    }



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    Story.instructions model.story (scene context model)


{-| Everything the chapter shows; `view` holds back the instructions while
the opening story plays.
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
            [ viewPool model.clock (waterLevel work.puzzle buffer)
            , Html.Lazy.lazy Editor.View.view buffer
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.pool.chooseTitle
                    , hint = context.texts.pool.chooseHint
                    , info = difficultyInfo context.texts
                    }
                    model.choice

              else
                Html.text ""
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context.texts model work.puzzle buffer
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys work.session)
        , position = Editor.View.position buffer
        }
    , keyboard =
        { target = Nothing
        , helpers =
            if model.difficulty == Nothing then
                [ "j", "k" ]

            else
                [ "h", "j", "k", "l", "w", "x", "d", "u" ]
        }
    }


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.pool.easyInfo

        Challenge ->
            texts.pool.challengeInfo

        Mad ->
            texts.pool.madInfo


viewPanel : Texts -> Model -> Puzzle -> Buffer -> Html msg
viewPanel texts model puzzle buffer =
    case model.difficulty of
        Nothing ->
            Html.div [ A.class "panel-content" ]
                [ Html.p [ A.class "big" ] [ Html.text texts.pool.chooseTitle ]
                , Html.p [] [ Html.text texts.pool.chooseHint ]
                ]

        Just difficulty ->
            let
                ( main, tips ) =
                    case hints texts.pool difficulty model puzzle buffer of
                        first :: rest ->
                            ( first, rest )

                        [] ->
                            ( "", [] )
            in
            Html.div [ A.class "panel-content" ]
                [ Html.span [ A.class "eyebrow" ] [ Html.text (Difficulty.name texts difficulty ++ " · " ++ texts.pool.idea) ]
                , Html.div [ A.class "columns" ]
                    [ Html.div [ A.class "column" ]
                        (Html.p [ A.class "big" ] [ Html.text main ]
                            :: List.map (\tip -> Html.p [ A.class "tip" ] [ Html.text tip ]) tips
                            ++ [ Html.p [ A.class "eyebrow" ] [ Html.text (texts.pool.wordsLeft (wordsToDelete puzzle buffer)) ] ]
                        )
                    , Checklist.goal texts.pool.goal puzzle.cleanPoem
                    ]
                ]


{-| Water, Alice swimming, the Mouse paddling about, tears falling. The
water stands at `level` (see `waterLevel`): it sinks as the poem gets
clean, and rises with every word the tide splashes in.
-}
viewPool : Float -> Float -> Svg msg
viewPool clock level =
    let
        seconds =
            clock / 1000

        -- How far below its highest the water stands; at most 12 pixels, so
        -- the Mouse stays in the picture.
        sink =
            12 * (1 - level)

        -- Each wave is drawn once (`wavePath`) and slid sideways; sliding by
        -- one wavelength looks exactly like the start again.
        wave row =
            Svg.g [ SA.transform ("translate(" ++ String.fromFloat (negate (waveShift seconds)) ++ " " ++ String.fromFloat sink ++ ")") ]
                [ Svg.Lazy.lazy waveLine row ]

        tear index =
            let
                x =
                    toFloat (modBy 300 (index * 83) + 10)

                y =
                    6 + fractional ((toFloat (modBy 40 (index * 29)) + seconds * 6 * toFloat (1 + modBy 3 index)) / 46) * 46
            in
            Svg.rect [ SA.x (String.fromFloat x), SA.y (String.fromFloat y), SA.width "1", SA.height "2", SA.fill "#dbbfef", SA.fillOpacity "0.5" ] []

        bob =
            sin (seconds * 2) * 2 + sink
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        (Svg.rect [ SA.width "320", SA.height "56", SA.fill "#2a1c35" ] []
            :: Svg.rect [ SA.y (String.fromFloat (4 + sink)), SA.width "320", SA.height "56", SA.fill "#1f3550" ] []
            :: List.map wave (List.range 0 4)
            ++ [ Sprite.viewAt { x = 120, y = 18 + bob, pixelSize = 2 } Sprites.aliceSwimming
               , Svg.rect [ SA.x "122", SA.y (String.fromFloat (34 + bob)), SA.width "20", SA.height "2", SA.fill "#6fa8dc" ] []
               , Sprite.viewAt { x = 200 + sin (seconds * 0.8) * 16, y = 27 + sin (seconds * 2.4) * 1.5 + sink, pixelSize = 2 } Sprites.mouse
               ]
            ++ List.map tear (List.range 0 7)
        )


{-| How far the waves have moved, within one wavelength.
-}
waveShift : Float -> Float
waveShift seconds =
    let
        wavelength =
            2 * pi * waveWidth
    in
    (seconds * 1.6 * waveWidth) - wavelength * toFloat (floor (seconds * 1.6 * waveWidth / wavelength))


waveWidth : Float
waveWidth =
    14


waveLine : Int -> Svg msg
waveLine row =
    Svg.path
        [ SA.d (wavePath row)
        , SA.stroke
            (if modBy 2 row == 0 then
                "#2a4a6e"

             else
                "#33577f"
            )
        , SA.fill "none"
        ]
        []


{-| One wave, a wavelength wider than the banner so it can slide.
-}
wavePath : Int -> String
wavePath row =
    List.range 0 104
        |> List.map
            (\i ->
                let
                    x =
                        toFloat (i * 4)

                    y =
                        8 + toFloat (row * 10) + sin (x / waveWidth + toFloat row) * 2
                in
                String.fromFloat x ++ " " ++ String.fromFloat y
            )
        |> String.join " L "
        |> (++) "M "


fractional : Float -> Float
fractional value =
    value - toFloat (floor value)
