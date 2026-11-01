module Chapter.LittleDoor exposing (Model, Msg(..), init, slipMilliseconds, update, view)

{-| Chapter 3, "The Little Door": Insert mode, taught with the golden key.

The little door behind the curtain has lots of locks: every gap and every
wrong letter (a wrong pin) in the lines below it. In Normal mode Alice holds
the key and walks with `h j k l` and counts. `i` puts the key into a lock:
Insert mode, where every key works the lock, `h j k l` too, because Alice is
bound to it. Esc pulls the key out again. On a wrong pin, `c` pulls the pin
out and puts the key straight in. Once every line reads right and the key is
out, the door opens on the garden.

  - Easy: three locks and one wrong pin.
  - Challenge: more locks and pins, all over the lines.
  - Mad: a longer passage, and the key slips out of the lock when the player
    waits more than `slipMilliseconds` while it is in.

Everything the player types uses only the home row (see `Texts.LittleDoorTexts`).

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
import Editor.Core as Core exposing (Command(..), Mode(..))
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
import Ui.Checklist as Checklist
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "03_little_door.txt"


{-| On mad, the key slips out of the lock after this long without a key, in
ms.
-}
slipMilliseconds : Float
slipMilliseconds =
    2000



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , work : Maybe Work
    , typedThisVisit : String
    , escTipShown : Bool
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
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , work = Nothing
    , typedThisVisit = ""
    , escTipShown = False
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
        Session.pressWith
            (Maybe.map (\keys -> { keys = keys, sinceLastKey = model.clock - model.lastKeyAt }) context.escapeKeys)
            isUnlocked
            key
            work.session
    of
        Session.Ran ran ->
            { model | work = Just { work | session = ran.session } }
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
        TypeChar char ->
            ( { model | typedThisVisit = model.typedThisVisit ++ String.fromChar char }, [ PlaySound TickSound ] )

        Backspace ->
            ( { model | backspaces = model.backspaces + 1, typedThisVisit = String.dropRight 1 model.typedThisVisit }, [ PlaySound StepSound ] )

        ExitInsert ->
            ( { model | typedThisVisit = "" }, [ PlaySound CorrectSound ] )

        InsertBefore ->
            putKeyIn model

        Change ->
            putKeyIn model

        _ ->
            ( model, [] )


{-| The key goes into a lock. The first time, tell them the Esc fallback:
some browsers keep Esc for themselves.
-}
putKeyIn : Model -> ( Model, List Effect )
putKeyIn model =
    ( { model | typedThisVisit = "", escTipShown = True }
    , if model.escTipShown then
        [ PlaySound PickupSound ]

      else
        [ PlaySound PickupSound, ShowToast (.caterpillar >> .escTip) ]
    )


{-| On mad, a key left in the lock too long slips out: back to Normal mode,
mid-word.
-}
slipIfWaiting : Model -> ( Model, List Effect )
slipIfWaiting model =
    case ( model.difficulty, model.work ) of
        ( Just Mad, Just work ) ->
            if isInsert work.session && Story.isPlaying model.story && model.clock - model.lastKeyAt > slipMilliseconds then
                ( { model
                    | work = Just { work | session = Session.apply ExitInsert work.session }
                    , slips = model.slips + 1
                    , typedThisVisit = ""
                  }
                , [ PlaySound BumpSound, ShowToast (.littleDoor >> .keySlipped) ]
                )

            else
                ( model, [] )

        _ ->
            ( model, [] )


{-| Done when every line reads right and the key is out: the door opens.
-}
finishIfOpen : Puzzle -> Session -> ( Model, List Effect ) -> ( Model, List Effect )
finishIfOpen puzzle session ( model, effects ) =
    if not (isInsert session) && Session.matches puzzle.answer session then
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


{-| One hint at a time, the one that helps now:

  - with the key in, after `h j k l` typed as if to walk: the key binds you;
  - with the key in otherwise: type, then Esc;
  - on a wrong pin: `c`;
  - else: find a lock.

-}
hint : Texts -> Model -> Work -> String
hint texts model work =
    if isInsert work.session then
        if walkedWithKeyIn model.typedThisVisit work.puzzle.answer then
            texts.littleDoor.hintBound

        else
            texts.littleDoor.hintTurn

    else if onWrongPin work then
        texts.littleDoor.hintPin

    else
        texts.littleDoor.hintFind


{-| The letters typed with the key in end in `h j k l` and fit no word of
the answer: the player tried to walk.
-}
walkedWithKeyIn : String -> String -> Bool
walkedWithKeyIn typed answer =
    let
        lastIsWalking =
            String.right 1 typed |> (\last -> last /= "" && String.contains last "hjkl")

        words =
            String.words (String.toLower answer)

        fitsAWord =
            List.any (\word -> String.contains typed word) words
    in
    lastIsWalking && not fitsAWord


{-| The cursor stands on a letter that differs from the answer, with the
letters around it matching: a wrong pin. Lines are compared from the start
and from the end, so a gap elsewhere on the line doesn't hide the pin.
-}
onWrongPin : Work -> Bool
onWrongPin work =
    let
        buffer =
            Session.buffer work.session

        ( lineNumber, column ) =
            Core.cursorLineColumn buffer

        nth list =
            list |> List.drop (lineNumber - 1) |> List.head |> Maybe.withDefault ""

        line =
            nth (String.lines (Core.toString buffer))

        target =
            nth (String.lines work.puzzle.answer)

        index =
            column - 1

        fromStart =
            String.left index line
                == String.left index target
                && String.slice index (index + 1) line
                /= String.slice index (index + 1) target
                && String.slice (index + 1) (index + 3) line
                == String.slice (index + 1) (index + 3) target

        after =
            String.dropLeft (index + 1) line

        targetIndex =
            String.length target - String.length after - 1

        fromEnd =
            targetIndex
                >= 0
                && String.endsWith after target
                && String.slice targetIndex (targetIndex + 1) target
                /= String.slice index (index + 1) line
                && String.slice (index - 2) index line
                == String.slice (targetIndex - 2) targetIndex target
    in
    index < String.length line && (fromStart || fromEnd)



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
            [ viewDoor
                { linesRight = lineMatches work
                , keyIn =
                    if isInsert work.session then
                        Just (Tuple.first (Core.cursorLineColumn buffer) - 1)

                    else
                        Nothing
                , open = model.doorOpen
                }
            , Html.Lazy.lazy viewEditor work.session
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
    , panel = viewPanel context model work
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys work.session)
        , position = Editor.View.position buffer
        }
    }


{-| Which lines of the work already read right, line by line.
-}
lineMatches : Work -> List Bool
lineMatches work =
    let
        clean =
            String.words >> String.join " "

        typed =
            String.lines (Core.toString (Session.buffer work.session))

        target =
            String.lines work.puzzle.answer
    in
    List.indexedMap
        (\index wanted ->
            (typed |> List.drop index |> List.head |> Maybe.map clean) == Just (clean wanted)
        )
        (List.filter (not << String.isEmpty << String.trim) target)


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.littleDoor.easyInfo

        Challenge ->
            texts.littleDoor.challengeInfo

        Mad ->
            texts.littleDoor.madInfo


viewPanel : Context -> Model -> Work -> Html msg
viewPanel context model work =
    let
        texts =
            context.texts

        right =
            List.length (List.filter identity (lineMatches work))

        total =
            List.length (lineMatches work)
    in
    case model.difficulty of
        Nothing ->
            Html.div [ A.class "panel-content" ]
                [ Html.p [ A.class "big" ] [ Html.text texts.littleDoor.chooseTitle ]
                , Html.p [] [ Html.text texts.littleDoor.chooseHint ]
                ]

        Just difficulty ->
            Html.div [ A.class "panel-content" ]
                [ Html.span [ A.class "eyebrow" ] [ Html.text (Difficulty.name texts difficulty ++ " · " ++ texts.littleDoor.linesRight right total) ]
                , Html.div [ A.class "columns" ]
                    [ Html.div [ A.class "column" ]
                        (Html.p [ A.class "big" ] [ Html.text (hint texts model work) ]
                            :: (case context.escapeKeys of
                                    Just ( first, second ) ->
                                        [ Html.p [ A.class "tip" ] [ Html.text (texts.escapeKeysWork (String.fromList [ first, second ])) ] ]

                                    Nothing ->
                                        []
                               )
                        )
                    , Checklist.goal texts.littleDoor.goal work.puzzle.answer
                    ]
                ]


{-| The hall's wall, the curtain drawn aside and the little door with one
keyhole per line: gold once that line reads right. With the key in, it
sticks out of the keyhole of the cursor's line; otherwise Alice holds it.
Once every lock is open, the door opens on the garden.
-}
viewDoor : { linesRight : List Bool, keyIn : Maybe Int, open : Bool } -> Svg msg
viewDoor { linesRight, keyIn, open } =
    let
        count =
            List.length linesRight

        perColumn =
            (count + 1) // 2

        holeAt index =
            { x =
                if index < perColumn then
                    204

                else
                    222
            , y = 14 + toFloat (modBy (max 1 perColumn) index) * (26 / toFloat (max 1 perColumn))
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
                [ Svg.rect [ SA.x (String.fromFloat at.x), SA.y (String.fromFloat at.y), SA.width "3", SA.height "2", SA.fill colour ] []
                , Svg.rect [ SA.x (String.fromFloat (at.x + 1)), SA.y (String.fromFloat (at.y + 2)), SA.width "1", SA.height "2", SA.fill colour ] []
                ]

        key =
            case keyIn of
                Just index ->
                    let
                        at =
                            holeAt (clamp 0 (count - 1) index)
                    in
                    Sprite.viewAt { x = at.x - 9, y = at.y, pixelSize = 1 } Sprites.goldenKey

                Nothing ->
                    Sprite.viewAt { x = 156, y = 33, pixelSize = 1 } Sprites.goldenKey
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        ([ Svg.rect [ SA.width "320", SA.height "56", SA.fill "#2d1f45" ] []
         , Svg.rect [ SA.y "48", SA.width "320", SA.height "8", SA.fill "#3b2a52" ] []

         -- The curtain, drawn aside.
         , Svg.rect [ SA.x "178", SA.y "2", SA.width "10", SA.height "46", SA.fill "#8c2a3c" ] []
         , Svg.rect [ SA.x "250", SA.y "2", SA.width "10", SA.height "46", SA.fill "#8c2a3c" ] []

         -- The door frame and the doorway.
         , Svg.rect [ SA.x "196", SA.y "6", SA.width "40", SA.height "42", SA.fill "#4a3420" ] []
         ]
            ++ (if open then
                    [ Svg.rect [ SA.x "199", SA.y "9", SA.width "34", SA.height "39", SA.fill "#5d9a4f" ] []
                    , Svg.rect [ SA.x "199", SA.y "9", SA.width "34", SA.height "14", SA.fill "#9fd0e8" ] []
                    , Svg.circle [ SA.cx "206", SA.cy "36", SA.r "2", SA.fill "#e8203f" ] []
                    , Svg.circle [ SA.cx "216", SA.cy "40", SA.r "2", SA.fill "#f4c15d" ] []
                    , Svg.circle [ SA.cx "226", SA.cy "35", SA.r "2", SA.fill "#ffffff" ] []
                    , Svg.rect [ SA.x "236", SA.y "9", SA.width "6", SA.height "39", SA.fill "#7a5634" ] []
                    ]

                else
                    Svg.rect [ SA.x "199", SA.y "9", SA.width "34", SA.height "39", SA.fill "#7a5634" ] []
                        :: List.indexedMap keyhole linesRight
               )
            ++ [ Sprite.viewAt { x = 140, y = 26, pixelSize = 1.1 } Sprites.alice ]
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
