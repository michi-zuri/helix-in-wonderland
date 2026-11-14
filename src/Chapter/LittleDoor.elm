module Chapter.LittleDoor exposing (Model, Msg(..), init, locks, slipMilliseconds, update, view)

{-| Chapter 3, "The Little Door": Insert mode, taught with the golden key.

The little door behind the curtain has a lock for every line below it. In
the lines, words are missing: a gap, or an emoji standing in for the word.
In Normal mode Alice holds the key and walks with `h j k l` and counts. `i`
puts the key into a lock: Insert mode, where every key is sent into the lock
as a letter, `h j k l` too. Esc pulls the key out again. On an emoji, `c`
lets it tumble out and puts the key straight in, so the word can be typed
(#107). Once every line reads right and the key is out, the door opens on
the garden.

  - Easy: one gap or emoji a line.
  - Challenge: several a line.
  - Mad: a longer passage, and the key slips out of the lock when the player
    waits more than `slipMilliseconds` while it is in.

Everything the player types uses only the home row (see `Texts.LittleDoorTexts`).

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
import Keyboard.Layout as Layout
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Texts exposing (Texts)
import Ui.Checklist as Checklist
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "03_little_door.txt"


{-| On mad, the key slips out of the lock after this long without a key, in
ms, and everything typed since it went in falls out with it (#109).
-}
slipMilliseconds : Float
slipMilliseconds =
    1000



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , work : Maybe Work
    , beforeKeyIn : Maybe Buffer
    , hintsGiven : List Hint
    , backspaces : Int
    , slips : Int
    , doorOpen : Bool
    , clock : Float
    , lastKeyAt : Float
    }


{-| The lines in the language the player started in (see `Chapter.Pool`).
-}
type alias Work =
    { puzzle : Puzzle
    , session : Session
    }


{-| The hints, each told once in a dialog, the first time it helps (#93).
-}
type Hint
    = FindALock
    | KeyIsIn
    | OnEmoji
    | KeySlipped


type alias Puzzle =
    { start : String
    , answer : String
    , ourBest : Maybe Int
    }


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.littleDoor >> .intro1))
            [ Dialog.say Alice (.littleDoor >> .intro2)
            , Dialog.say Narrator (.littleDoor >> .intro3)
            , Dialog.say Narrator (.littleDoor >> .intro4)
            , Dialog.say Narrator (.littleDoor >> .intro5)
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , work = Nothing
    , beforeKeyIn = Nothing
    , hintsGiven = []
    , backspaces = 0
    , slips = 0
    , doorOpen = False
    , clock = 0
    , lastKeyAt = 0
    }


puzzleFor : Texts -> Difficulty -> Puzzle
puzzleFor texts difficulty =
    case difficulty of
        Easy ->
            { start = texts.littleDoor.easyStart, answer = texts.littleDoor.easyAnswer, ourBest = Nothing }

        Challenge ->
            { start = texts.littleDoor.challengeStart, answer = texts.littleDoor.challengeAnswer, ourBest = Nothing }

        Mad ->
            { start = texts.littleDoor.madStart, answer = texts.littleDoor.madAnswer, ourBest = Just texts.littleDoor.madBest }


{-| The text follows the language setting until a difficulty is picked.
-}
currentWork : Texts -> Model -> Work
currentWork texts model =
    case model.work of
        Just work ->
            work

        Nothing ->
            let
                puzzle =
                    puzzleFor texts (Maybe.withDefault Easy model.difficulty)
            in
            { puzzle = puzzle, session = Session.start puzzle.start }


isInsert : Session -> Bool
isInsert session =
    Core.mode (Session.buffer session) == Insert



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
                                -- Remember when: for the escape keys' timeout, and for mad's slipping key.
                                edit context key model
                                    |> Tuple.mapFirst (\edited -> { edited | lastKeyAt = edited.clock })
                                    |> giveHints context

                            Nothing ->
                                choose context.texts key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    -- Reading counts as pressing a key: on mad, the key doesn't slip while a hint is read.
                    ( { model | story = story, lastKeyAt = model.clock }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story, choice = DifficultyChoice.start model.clock }, [] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score context.texts model) ] )

        Frame milliseconds ->
            { model | clock = model.clock + milliseconds, story = Story.tick milliseconds model.story }
                |> slipIfWaiting


choose : Texts -> String -> Model -> ( Model, List Effect )
choose texts key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            let
                puzzle =
                    puzzleFor texts difficulty
            in
            ( { model | difficulty = Just difficulty, work = Just { puzzle = puzzle, session = Session.start puzzle.start } }
                |> tell FindALock [ Dialog.say Narrator (.littleDoor >> .hintFind) ]
            , [ PlaySound PickupSound ]
            )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| What the player knows by now: walking with `h j k l` and counts, and
this chapter's `i`, Esc and `c`. Everything else comes later.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    case command of
        MoveLeft ->
            True

        MoveRight ->
            True

        MoveDown ->
            True

        MoveUp ->
            True

        InsertBefore ->
            True

        ExitInsert ->
            True

        Change ->
            True

        TypeChar _ ->
            True

        Backspace ->
            True

        CollapseSelection ->
            True

        _ ->
            False


edit : Context -> String -> Model -> ( Model, List Effect )
edit context key model =
    let
        work =
            currentWork context.texts model
    in
    case
        Session.press isUnlocked key work.session
    of
        Session.Ran ran ->
            { model
                | work = Just { work | session = ran.session }
                , beforeKeyIn =
                    -- What the lines were before the key went in, for mad's slipping key.
                    if ran.command == InsertBefore || ran.command == Change then
                        Just ran.before

                    else if ran.command == ExitInsert then
                        Nothing

                    else
                        model.beforeKeyIn
            }
                |> notice ran.command
                |> finishIfOpen work.puzzle ran.session

        Session.Waiting session ->
            ( { model | work = Just { work | session = session } }, [] )

        Session.Locked session ->
            ( dropCount work session model, [ ShowToast .notYet ] )

        Session.Ignored session ->
            ( dropCount work session model, [] )


notice : Command -> Model -> ( Model, List Effect )
notice command model =
    case command of
        TypeChar _ ->
            ( model, [ PlaySound TickSound ] )

        Backspace ->
            ( { model | backspaces = model.backspaces + 1 }, [ PlaySound StepSound ] )

        ExitInsert ->
            ( model, [ PlaySound CorrectSound ] )

        InsertBefore ->
            putKeyIn model

        Change ->
            putKeyIn model

        _ ->
            ( model, [] )


putKeyIn : Model -> ( Model, List Effect )
putKeyIn model =
    ( model, [ PlaySound PickupSound ] )


{-| On mad, a key left in the lock too long slips out: back to Normal mode,
mid-word.
-}
slipIfWaiting : Model -> ( Model, List Effect )
slipIfWaiting model =
    case ( model.difficulty, model.work ) of
        ( Just Mad, Just work ) ->
            if isInsert work.session && Story.isPlaying model.story && model.clock - model.lastKeyAt > slipMilliseconds then
                let
                    -- Everything typed since the key went in falls out with it (#109).
                    session =
                        case model.beforeKeyIn of
                            Just before ->
                                Session.withBuffer before work.session

                            Nothing ->
                                Session.apply ExitInsert work.session

                    slipped =
                        { model
                            | work = Just { work | session = session }
                            , slips = model.slips + 1
                            , beforeKeyIn = Nothing
                        }
                in
                -- The first time it is told in a dialog, later only noted.
                if List.member KeySlipped model.hintsGiven then
                    ( slipped, [ PlaySound BumpSound, ShowToast (.littleDoor >> .keySlipped) ] )

                else
                    ( tell KeySlipped [ Dialog.say Narrator (.littleDoor >> .keySlipped) ] slipped, [ PlaySound BumpSound ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )


{-| Done when every line reads right and the key is out: the door opens.
-}
finishIfOpen : Puzzle -> Session -> ( Model, List Effect ) -> ( Model, List Effect )
finishIfOpen puzzle session ( model, effects ) =
    if not (isInsert session) && Session.matchesExactly puzzle.answer session then
        ( { model
            | doorOpen = True
            , story =
                Story.conclude (Dialog.say Narrator (.littleDoor >> .finale1))
                    [ Dialog.say Narrator (.littleDoor >> .finale2) ]
          }
        , effects ++ [ PlaySound PortalSound ]
        )

    else
        ( model, effects )


{-| The difficulty sets the most tarts there are (easy 1, challenge 2,
mad 3); many Backspaces can cost some of them.
-}
score : Texts -> Model -> Contract.Score
score texts model =
    let
        difficulty =
            Maybe.withDefault Easy model.difficulty

        work =
            currentWork texts model

        forBackspaces =
            if model.backspaces <= 3 then
                3

            else if model.backspaces <= 8 then
                2

            else
                1
    in
    { tarts = min (Difficulty.tarts difficulty) forBackspaces
    , stats =
        List.concat
            [ [ { label = .results >> .difficulty, value = \t -> Difficulty.name t difficulty }
              , { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes work.session) }
              ]
            , case work.puzzle.ourBest of
                Just best ->
                    [ { label = .littleDoor >> .ourBest, value = \_ -> String.fromInt best } ]

                Nothing ->
                    []
            , [ { label = .results >> .backspaces, value = \_ -> String.fromInt model.backspaces } ]
            , if difficulty == Mad then
                [ { label = .littleDoor >> .slips, value = \_ -> String.fromInt model.slips } ]

              else
                []
            ]
    }



-- HINTS


{-| Each hint is told once, in a dialog, the first time it helps (#93):

  - after picking the difficulty: find a lock (see `choose`);
  - the key goes in for the first time: type, then Esc (and the ways to
    Esc when the browser keeps it);
  - Alice stands on an emoji: `c`;
  - on mad, the key slips out (see `slipIfWaiting`).

-}
giveHints : Context -> ( Model, List Effect ) -> ( Model, List Effect )
giveHints context ( model, effects ) =
    case model.work of
        Just work ->
            if model.doorOpen then
                ( model, effects )

            else if isInsert work.session then
                -- One line: type the letters (h k l too), out with Esc or j k
                -- (#110, #113, #117); Ctrl+[ is the fallback for browsers that
                -- keep Esc, told only where [ is a plain key.
                ( tell KeyIsIn
                    (Dialog.say Narrator (.littleDoor >> .hintTurn)
                        :: (if Layout.typesBracketEasily context.layout then
                                [ Dialog.say Narrator (.caterpillar >> .escTip) ]

                            else
                                []
                           )
                    )
                    model
                , effects
                )

            else if onEmoji work then
                ( tell OnEmoji [ Dialog.say Narrator (.littleDoor >> .hintPin) ] model, effects )

            else
                ( model, effects )

        Nothing ->
            ( model, effects )


{-| Tell a hint in a dialog, unless it was told before.
-}
tell : Hint -> List Dialog.Line -> Model -> Model
tell hint lines model =
    case lines of
        first :: rest ->
            if List.member hint model.hintsGiven then
                model

            else
                { model
                    | hintsGiven = hint :: model.hintsGiven
                    , story = Story.tell first rest model.story
                }

        [] ->
            model


{-| The cursor stands on an emoji: a stand-in for a word, which `c` replaces
(#107). It is a character the goal doesn't have anywhere. Characters are
taken as a list, since an emoji is one character to the editor but two
units to Elm's `String` functions.
-}
onEmoji : Work -> Bool
onEmoji work =
    let
        buffer =
            Session.buffer work.session

        ( lineNumber, column ) =
            Core.cursorLineColumn buffer
    in
    String.lines (Core.toString buffer)
        |> List.drop (lineNumber - 1)
        |> List.head
        |> Maybe.map (String.toList >> List.drop (column - 1) >> List.head)
        |> Maybe.andThen identity
        |> Maybe.map (\char -> not (Char.isAlphaNum char) && not (String.contains (String.fromChar char) work.puzzle.answer))
        |> Maybe.withDefault False



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    Story.instructions model.story (scene context model)


{-| Everything the chapter shows. There is no panel (#93): hints come in
dialogs, and the goal stands under the lines in the editor, so everything
fits on one screen. The status line is Helix's own.
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
            [ viewDoor
                { linesRight = lineMatches work
                , keyIn =
                    if isInsert work.session then
                        Just (Tuple.first (Core.cursorLineColumn buffer) - 1)

                    else
                        Nothing
                , open = model.doorOpen
                }
            , viewKeyMenu context.texts work.session
            , Html.div [ A.class "editor-with-goal" ]
                [ Html.Lazy.lazy viewEditor work.session
                , if model.difficulty == Nothing then
                    Html.text ""

                  else
                    Checklist.goal context.texts.littleDoor.goal work.puzzle.answer
                ]
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.littleDoor.chooseTitle
                    , hint = context.texts.littleDoor.chooseHint
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


{-| One lock per line of the text, gold while that line reads exactly like
its line of the goal, spaces included (#103). With fewer lines than the goal
there are fewer locks. Lines beyond the goal add one lock that never opens,
however many there are (#105): it goes away only with the extra lines. The
door opens on exactly the goal's number of locks, all gold
(`Session.matchesExactly`).
-}
lineMatches : Work -> List Bool
lineMatches work =
    let
        target =
            Session.exactLines work.puzzle.answer

        typed =
            Session.exactLines (Core.toString (Session.buffer work.session))

        locksForLines =
            List.map2 (==) typed target
    in
    if List.length typed > List.length target then
        locksForLines ++ [ False ]

    else
        locksForLines


{-| The locks on the door right now, for tests: one per line, `True` when
gold.
-}
locks : Texts -> Model -> List Bool
locks texts model =
    lineMatches (currentWork texts model)


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.littleDoor.easyInfo

        Challenge ->
            texts.littleDoor.challengeInfo

        Mad ->
            texts.littleDoor.madInfo


{-| The hall's wall, a low curtain drawn aside and the little door, about
fifteen inches high as in the book: half as tall as Alice, who towers over
it. One keyhole per line, gold once that line reads right. With the key in,
it sticks out of the keyhole of the cursor's line; otherwise Alice holds it.
Once every lock is open, the door opens on the garden.
-}
viewDoor : { linesRight : List Bool, keyIn : Maybe Int, open : Bool } -> Svg msg
viewDoor { linesRight, keyIn, open } =
    let
        count =
            List.length linesRight

        -- At most 5 keyholes a column, each 3 pixels tall with a 1-pixel
        -- gap, and as many columns as needed (at least two), drawn closer
        -- together the more there are: every keyhole stays countable (#105).
        columns =
            max 2 ((count + 4) // 5)

        perColumn =
            max 1 ((count + columns - 1) // columns)

        columnGap =
            min 6 (12 / toFloat (columns - 1))

        top =
            28 + (20 - toFloat (perColumn * 4 - 1)) / 2

        holeAt index =
            { x = 114 - columnGap * toFloat (columns - 1) / 2 + columnGap * toFloat (index // perColumn)
            , y = top + 4 * toFloat (modBy perColumn index)
            }

        keyhole index isRight =
            let
                at =
                    holeAt index

                colour =
                    if isRight then
                        "#efba5d"

                    else
                        "#1b1226"
            in
            Svg.g []
                [ Svg.rect [ SA.x (String.fromFloat at.x), SA.y (String.fromFloat at.y), SA.width "2", SA.height "2", SA.fill colour ] []
                , Svg.rect [ SA.x (String.fromFloat (at.x + 0.5)), SA.y (String.fromFloat (at.y + 2)), SA.width "1", SA.height "1", SA.fill colour ] []
                ]

        key =
            case keyIn of
                Just index ->
                    let
                        at =
                            holeAt (clamp 0 (count - 1) index)
                    in
                    Sprite.viewAt { x = at.x - 5, y = at.y + 0.5, pixelSize = 0.6 } Sprites.goldenKey

                Nothing ->
                    Sprite.viewAt { x = 88, y = 30, pixelSize = 1 } Sprites.goldenKey
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        ([ Svg.rect [ SA.width "320", SA.height "56", SA.fill "#2d1f45" ] []
         , Svg.rect [ SA.y "48", SA.width "320", SA.height "8", SA.fill "#3b2a52" ] []

         -- The low curtain, drawn aside.
         , Svg.rect [ SA.x "98", SA.y "20", SA.width "7", SA.height "28", SA.fill "#8c2a3c" ] []
         , Svg.rect [ SA.x "127", SA.y "20", SA.width "7", SA.height "28", SA.fill "#8c2a3c" ] []

         -- The door frame and the doorway: about half as tall as Alice.
         , Svg.rect [ SA.x "106", SA.y "26", SA.width "18", SA.height "22", SA.fill "#4a3420" ] []
         ]
            ++ (if open then
                    [ Svg.rect [ SA.x "108", SA.y "28", SA.width "14", SA.height "20", SA.fill "#5d9a4f" ] []
                    , Svg.rect [ SA.x "108", SA.y "28", SA.width "14", SA.height "7", SA.fill "#9fd0e8" ] []
                    , Svg.circle [ SA.cx "111", SA.cy "41", SA.r "1", SA.fill "#e8203f" ] []
                    , Svg.circle [ SA.cx "115", SA.cy "44", SA.r "1", SA.fill "#f4c15d" ] []
                    , Svg.circle [ SA.cx "119", SA.cy "40", SA.r "1", SA.fill "#ffffff" ] []
                    , Svg.rect [ SA.x "124", SA.y "28", SA.width "3", SA.height "20", SA.fill "#7a5634" ] []
                    ]

                else
                    Svg.rect [ SA.x "108", SA.y "28", SA.width "14", SA.height "20", SA.fill "#7a5634" ] []
                        :: List.indexedMap keyhole linesRight
               )
            -- Alice, far too big for the door, on the left: the messages at the top
            -- of the stage never cover her.
            ++ [ Sprite.viewAt { x = 56, y = 4, pixelSize = 2.75 } Sprites.alice ]
            ++ (if open then
                    []

                else
                    [ key ]
               )
        )


{-| A key that ran nothing drops a count typed before it. Otherwise the
model stays as it was, so the text keeps following the language setting.
-}
dropCount : Work -> Session -> Model -> Model
dropCount work session model =
    if not (Session.isWaiting work.session) then
        model

    else
        { model | work = Just { work | session = session } }


viewEditor : Session -> Html msg
viewEditor session =
    Editor.View.viewWith { labels = [], marks = [] } (Session.buffer session)


{-| While a `j` in INSERT waits, the little menu of what comes next (#110).
-}
viewKeyMenu : Texts -> Session -> Html msg
viewKeyMenu texts session =
    if Session.isAfterJ session then
        Editor.View.keyMenu [ ( "k", texts.keyMenu.normal ), ( "j", texts.keyMenu.typeJ ) ]

    else
        Html.text ""
