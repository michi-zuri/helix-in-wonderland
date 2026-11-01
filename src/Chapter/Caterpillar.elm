module Chapter.Caterpillar exposing (Model, Msg(..), init, update, view)

{-| Chapter 6, "Advice from a Caterpillar": Insert mode. Typing practice
and editing finally meet.

  - Easy: words are missing from the Caterpillar's lines; move to each gap,
    press `i`, type the word, go back to Normal mode with Esc.
  - Challenge: gaps, a word missing at the end of a line (`a`), and wrong
    words to change (`w` then `c`).
  - Mad: Alice recites "You are old, Father William" to the Caterpillar and
    gets it wrong, as in the book: all of the above, plus a line that
    doesn't belong (`x` then `d`).

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
import Pixel.Shapes as Shapes
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy
import Texts exposing (Texts)
import Ui.Checklist as Checklist
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "06_caterpillar.txt"



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , work : Maybe Work
    , enteredInsert : Bool
    , typedInInsert : Bool
    , typedThisVisit : Bool
    , leftInsert : Bool
    , escTipShown : Bool
    , backspaces : Int
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
    { gaps : String
    , answer : String
    , ourBest : Maybe Int
    }


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.caterpillar >> .intro1))
            [ Dialog.say Caterpillar (.caterpillar >> .intro2)
            , Dialog.say Narrator (.caterpillar >> .intro3)
            , Dialog.say Narrator (.caterpillar >> .intro4)
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , work = Nothing
    , enteredInsert = False
    , typedInInsert = False
    , typedThisVisit = False
    , leftInsert = False
    , escTipShown = False
    , backspaces = 0
    , clock = 0
    , lastKeyAt = 0
    }


puzzleFor : Texts -> Difficulty -> Puzzle
puzzleFor texts difficulty =
    case difficulty of
        Easy ->
            { gaps = texts.caterpillar.gaps, answer = texts.caterpillar.answer, ourBest = Nothing }

        Challenge ->
            { gaps = texts.caterpillar.challengeStart, answer = texts.caterpillar.challengeAnswer, ourBest = Nothing }

        Mad ->
            { gaps = texts.caterpillar.madStart, answer = texts.caterpillar.madAnswer, ourBest = Just texts.caterpillar.madBest }


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
            { puzzle = puzzle, session = Session.start puzzle.gaps }



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
                                -- Remember when, for the escape keys' timeout.
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
            ( { model | clock = model.clock + milliseconds, story = Story.tick milliseconds model.story }, [] )


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
            ( { model | difficulty = Just difficulty, work = Just { puzzle = puzzle, session = Session.start puzzle.gaps } }
            , [ PlaySound PickupSound ]
            )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| `e` and `b` wait for a later chapter, as in chapter 5.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    not (List.member command [ SelectNextWordEnd, SelectPreviousWordStart ])


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
                |> finishIfAnswered work.puzzle ran.session

        Session.Waiting session ->
            ( { model | work = Just { work | session = session } }, [] )

        Session.Locked session ->
            ( dropCount work session model, [] )

        Session.Ignored session ->
            ( dropCount work session model, [] )


notice : Command -> Model -> ( Model, List Effect )
notice command model =
    case command of
        TypeChar _ ->
            ( { model | typedInInsert = True, typedThisVisit = True }, [ PlaySound TickSound ] )

        Backspace ->
            ( { model | backspaces = model.backspaces + 1 }, [ PlaySound StepSound ] )

        ExitInsert ->
            ( { model | leftInsert = model.leftInsert || model.typedThisVisit }, [ PlaySound CorrectSound ] )

        _ ->
            if List.member command [ InsertBefore, InsertAfter, Change ] then
                enterInsert model

            else
                ( model, [] )


{-| The first time someone enters Insert mode, tell them the Esc fallback:
some browsers keep Esc for themselves.
-}
enterInsert : Model -> ( Model, List Effect )
enterInsert model =
    ( { model | enteredInsert = True, typedThisVisit = False, escTipShown = True }
    , if model.escTipShown then
        [ PlaySound TickSound ]

      else
        [ PlaySound TickSound, ShowToast (.caterpillar >> .escTip) ]
    )


{-| Done when the lines read right and the player is back in Normal mode.
-}
finishIfAnswered : Puzzle -> Session -> ( Model, List Effect ) -> ( Model, List Effect )
finishIfAnswered puzzle session ( model, effects ) =
    if Core.mode (Session.buffer session) == Normal && Session.matches puzzle.answer session then
        ( { model
            | story =
                Story.conclude (Dialog.say Caterpillar (.caterpillar >> .finale1))
                    [ Dialog.say Narrator (.caterpillar >> .finale2) ]
          }
        , effects ++ [ PlaySound PickupSound ]
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
                    [ { label = .caterpillar >> .ourBest, value = \_ -> String.fromInt best } ]

                Nothing ->
                    []
            , [ { label = .results >> .backspaces, value = \_ -> String.fromInt model.backspaces } ]
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
            [ viewMushroom model.clock
            , Html.Lazy.lazy viewEditor work.session
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.caterpillar.chooseTitle
                    , hint = context.texts.caterpillar.chooseHint
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


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.caterpillar.easyInfo

        Challenge ->
            texts.caterpillar.challengeInfo

        Mad ->
            texts.caterpillar.madInfo


{-| The hint for challenge and mad, one at a time: what to do with a
selection, at the end of a line, while typing, or else how to find what
differs from the goal.
-}
hint : Texts -> Session -> String
hint texts session =
    let
        buffer =
            Session.buffer session

        ( first, last ) =
            Core.selectionRange buffer

        text =
            Core.toString buffer

        atLineEnd =
            String.slice (Core.head buffer + 1) (Core.head buffer + 2) text == "\n"
    in
    if Core.mode buffer == Insert then
        texts.caterpillar.hintTyping

    else if first /= last then
        texts.caterpillar.hintChange

    else if atLineEnd then
        texts.caterpillar.hintAppend

    else
        texts.caterpillar.hintFind


viewPanel : Context -> Model -> Work -> Html msg
viewPanel context model work =
    let
        texts =
            context.texts

        filled =
            Session.matchingLines work.puzzle.answer work.session

        backInNormal =
            Core.mode (Session.buffer work.session) == Normal

        instructions =
            case model.difficulty of
                Just Easy ->
                    [ Checklist.view
                        [ ( model.enteredInsert, texts.caterpillar.pressI )
                        , ( model.typedInInsert, texts.caterpillar.typeWord )
                        , ( model.leftInsert, texts.caterpillar.pressEsc )
                        , ( filled == 3 && backInNormal, texts.caterpillar.fillAll filled )
                        ]
                    , Html.p [ A.class "eyebrow" ] [ Html.text texts.caterpillar.tip ]
                    ]

                _ ->
                    [ Html.p [ A.class "big" ] [ Html.text (hint texts work.session) ] ]
    in
    case model.difficulty of
        Nothing ->
            Html.div [ A.class "panel-content" ]
                [ Html.p [ A.class "big" ] [ Html.text texts.caterpillar.chooseTitle ]
                , Html.p [] [ Html.text texts.caterpillar.chooseHint ]
                ]

        Just difficulty ->
            viewWorkPanel context difficulty instructions work


viewWorkPanel : Context -> Difficulty -> List (Html msg) -> Work -> Html msg
viewWorkPanel context difficulty instructions work =
    let
        texts =
            context.texts
    in
    Html.div [ A.class "panel-content" ]
        [ Html.span [ A.class "eyebrow" ] [ Html.text (Difficulty.name texts difficulty) ]
        , Html.div [ A.class "columns" ]
            [ Html.div [ A.class "column" ]
                (instructions
                    ++ (case context.escapeKeys of
                            Just ( first, second ) ->
                                [ Html.p [ A.class "tip" ] [ Html.text (texts.escapeKeysWork (String.fromList [ first, second ])) ] ]

                            Nothing ->
                                []
                       )
                )
            , Checklist.goal texts.caterpillar.goal work.puzzle.answer
            ]
        ]


{-| Grass, the Caterpillar on its mushroom blowing smoke rings, and a small
Alice looking up at it.
-}
viewMushroom : Float -> Svg msg
viewMushroom clock =
    let
        seconds =
            clock / 1000

        -- The grass is drawn once and leans as a whole, pivoting on the ground.
        sway =
            sin (seconds * 1.5) * 6

        ring index =
            let
                progress =
                    fractional (seconds * 0.35 + toFloat index / 3)
            in
            Svg.circle
                [ SA.cx (String.fromFloat (204 - progress * 26))
                , SA.cy (String.fromFloat (14 - progress * 12))
                , SA.r (String.fromFloat (2 + progress * 5))
                , SA.fill "none"
                , SA.stroke "#dbbfef"
                , SA.strokeOpacity (String.fromFloat (0.5 * (1 - progress)))
                ]
                []
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        ([ Svg.rect [ SA.width "320", SA.height "56", SA.fill "#2d1f45" ] []
         , Svg.rect [ SA.y "44", SA.width "320", SA.height "12", SA.fill "#3a5a3a" ] []
         ]
            ++ [ Svg.g [ SA.transform ("translate(0 44) skewX(" ++ String.fromFloat sway ++ ") translate(0 -44)") ]
                    [ Svg.Lazy.lazy grass () ]
               ]
            ++ [ Sprite.viewAt { x = 196, y = 12, pixelSize = 2 } Sprites.caterpillar
               , Sprite.viewAt { x = 140, y = 26, pixelSize = 1.1 } Sprites.alice
               ]
            ++ List.map ring [ 0, 1, 2 ]
        )


grass : () -> Svg msg
grass () =
    Shapes.view
        (Shapes.layers
            (List.map
                (\x ->
                    let
                        height =
                            toFloat (3 + modBy 6 (x * 7))
                    in
                    { x = toFloat x
                    , y = 44 - height
                    , width = 1
                    , height = height
                    , colour =
                        if modBy 2 x == 0 then
                            "#4f7d4a"

                        else
                            "#6fae62"
                    }
                )
                (List.range 0 106 |> List.map ((*) 3))
            )
        )


fractional : Float -> Float
fractional value =
    value - toFloat (floor value)


{-| A key that ran nothing drops a count typed before it. Otherwise the
model stays as it was, so the text keeps following the language setting.
-}
dropCount : Work -> Session -> Model -> Model
dropCount work session model =
    if not (Session.isWaiting work.session) then
        model

    else
        { model | work = Just { work | session = session } }


{-| The editor with `gw`'s labels, drawn again only when the session
changes (it is lazy on the session, which only changes on a key).
-}
viewEditor : Session -> Html msg
viewEditor session =
    Editor.View.viewWith { labels = Session.jumpLabels session, marks = [] } (Session.buffer session)
