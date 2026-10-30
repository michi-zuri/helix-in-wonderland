module Chapter.Caterpillar exposing (Model, Msg(..), init, update, view)

{-| Chapter 5, "Advice from a Caterpillar": Insert mode. Words are missing
from the Caterpillar's lines; the player moves to each gap, presses `i` (or
`a`), types the word and returns to Normal mode with Esc. Typing practice
and editing finally meet.
-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Editor.Core as Core exposing (Buffer, Command(..), Mode(..))
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


file : String
file =
    "05_caterpillar.txt"



-- MODEL


type alias Model =
    { story : Story
    , work : Maybe Work
    , enteredInsert : Bool
    , typedInInsert : Bool
    , typedThisVisit : Bool
    , leftInsert : Bool
    , escTipShown : Bool
    , backspaces : Int
    , clock : Float
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
    }


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.caterpillar >> .intro1))
            [ Dialog.say Caterpillar (.caterpillar >> .intro2)
            , Dialog.say Narrator (.caterpillar >> .intro3)
            , Dialog.say Narrator (.caterpillar >> .intro4)
            ]
    , work = Nothing
    , enteredInsert = False
    , typedInInsert = False
    , typedThisVisit = False
    , leftInsert = False
    , escTipShown = False
    , backspaces = 0
    , clock = 0
    }


currentWork : Texts -> Model -> Work
currentWork texts model =
    case model.work of
        Just work ->
            work

        Nothing ->
            { puzzle = { gaps = texts.caterpillar.gaps, answer = texts.caterpillar.answer }
            , session = Session.start texts.caterpillar.gaps
            }



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
                        edit context.texts key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    ( { model | story = story }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story }, [] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score context.texts model) ] )

        Frame milliseconds ->
            ( { model | clock = model.clock + milliseconds, story = Story.tick milliseconds model.story }, [] )


edit : Texts -> String -> Model -> ( Model, List Effect )
edit texts key model =
    let
        work =
            currentWork texts model
    in
    case Session.press (always True) key work.session of
        Session.Ran ran ->
            { model | work = Just { work | session = ran.session } }
                |> notice ran.command
                |> Tuple.mapSecond ((::) (FlashKey key True))
                |> finishIfAnswered work.puzzle ran.session

        Session.Counting session ->
            ( { model | work = Just { work | session = session } }, [ FlashKey key True ] )

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


score : Texts -> Model -> Contract.Score
score texts model =
    { tarts =
        if model.backspaces <= 3 then
            3

        else if model.backspaces <= 8 then
            2

        else
            1
    , stats =
        [ { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes (currentWork texts model).session) }
        , { label = .results >> .backspaces, value = \_ -> String.fromInt model.backspaces }
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
            , Html.Lazy.lazy Editor.View.view buffer
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context.texts model work
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys work.session)
        , position = Editor.View.position buffer
        }
    , keyboard = { target = Nothing, helpers = helpers buffer }
    }


{-| In Normal mode the editing keys are outlined; in Insert mode, Esc.
-}
helpers : Buffer -> List String
helpers buffer =
    case Core.mode buffer of
        Normal ->
            [ "h", "j", "k", "l", "w", "e", "b", "i", "a", "c", "d", "u" ]

        Insert ->
            [ "Escape" ]


viewPanel : Texts -> Model -> Work -> Html msg
viewPanel texts model work =
    let
        filled =
            Session.matchingLines work.puzzle.answer work.session

        backInNormal =
            Core.mode (Session.buffer work.session) == Normal
    in
    Html.div [ A.class "panel-content" ]
        [ Html.div [ A.class "columns" ]
            [ Html.div [ A.class "column" ]
                [ Checklist.view
                    [ ( model.enteredInsert, texts.caterpillar.pressI )
                    , ( model.typedInInsert, texts.caterpillar.typeWord )
                    , ( model.leftInsert, texts.caterpillar.pressEsc )
                    , ( filled == 3 && backInNormal, texts.caterpillar.fillAll filled )
                    ]
                , Html.p [ A.class "eyebrow" ] [ Html.text texts.caterpillar.tip ]
                ]
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
    if Session.pendingCount work.session == "" then
        model

    else
        { model | work = Just { work | session = session } }
