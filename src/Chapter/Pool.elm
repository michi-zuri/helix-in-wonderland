module Chapter.Pool exposing (Model, Msg(..), init, update, view)

{-| Chapter 4, "The Pool of Tears": the big Helix idea, select first and
then act. The player cleans junk words out of Carroll's crocodile poem with
`w e b` (select a word), `x` (select a line), `d` (delete) and `u` (undo).

Insert-mode keys stay locked until chapter 5.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Editor.Core as Core exposing (Buffer, Command(..))
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
import Svg.Lazy
import Texts exposing (PoolTexts, Texts)
import Ui.Checklist as Checklist
import Ui.Dialog as Dialog


file : String
file =
    "04_pool_of_tears.txt"



-- MODEL


type alias Model =
    { story : Story
    , work : Maybe Work
    , reachedJunk : Bool
    , selectedSomething : Bool
    , deletedJunk : Bool
    , wrongDeletions : Int
    , clock : Float
    }


{-| The poem in the language the player started in. Until the first key
press it follows the language setting, so switching language before
starting just shows the other poem.
-}
type alias Work =
    { puzzle : Puzzle
    , session : Session
    }


type alias Puzzle =
    { poem : String
    , cleanPoem : String
    , junk : String
    , flood : String
    }


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.pool >> .intro1))
            [ Dialog.say Alice (.pool >> .intro2)
            , Dialog.say Narrator (.pool >> .intro3)
            , Dialog.say Narrator (.pool >> .intro4)
            ]
    , work = Nothing
    , reachedJunk = False
    , selectedSomething = False
    , deletedJunk = False
    , wrongDeletions = 0
    , clock = 0
    }


currentWork : Texts -> Model -> Work
currentWork texts model =
    case model.work of
        Just work ->
            work

        Nothing ->
            let
                puzzle =
                    puzzleFrom texts.pool
            in
            { puzzle = puzzle, session = Session.start puzzle.poem }


puzzleFrom : PoolTexts -> Puzzle
puzzleFrom texts =
    { poem = texts.poem, cleanPoem = texts.cleanPoem, junk = texts.junk, flood = texts.flood }



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


{-| Everything except inserting text, which chapter 5 teaches.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    not (List.member command [ InsertBefore, InsertAfter, Change ])


edit : Texts -> String -> Model -> ( Model, List Effect )
edit texts key model =
    let
        work =
            currentWork texts model
    in
    case Session.press isUnlocked key work.session of
        Session.Ran ran ->
            { model | work = Just { work | session = ran.session } }
                |> notice work.puzzle ran
                |> Tuple.mapSecond ((::) (FlashKey key True))
                |> finishIfClean work.puzzle ran.session

        Session.Locked ->
            ( model, [ ShowToast .notYet, PlaySound ErrorSound, FlashKey key False ] )

        Session.Ignored ->
            ( model, [] )


{-| Tick off objectives and judge deletions.
-}
notice : Puzzle -> { command : Command, before : Buffer, session : Session } -> Model -> ( Model, List Effect )
notice puzzle ran model =
    let
        buffer =
            Session.buffer ran.session

        noticed =
            { model
                | reachedJunk = model.reachedJunk || touchesJunk puzzle buffer
                , selectedSomething = model.selectedSomething || isSelection ran.command
            }
    in
    if ran.command == Delete then
        judgeDeletion puzzle (Core.selectedText ran.before) noticed

    else
        ( noticed, [] )


{-| Deleting junk is good. Deleting a word of the poem counts as a mistake
(the child is told to press `u`). Deleting only spaces is neither.
-}
judgeDeletion : Puzzle -> String -> Model -> ( Model, List Effect )
judgeDeletion puzzle deleted model =
    let
        words =
            wordsIn deleted

        isJunk word =
            word == puzzle.junk || word == puzzle.flood
    in
    if List.isEmpty words then
        ( model, [] )

    else if List.all isJunk words then
        ( { model | deletedJunk = model.deletedJunk || List.member puzzle.junk words }, [ PlaySound CorrectSound ] )

    else
        ( { model | wrongDeletions = model.wrongDeletions + 1 }
        , [ ShowToast (.pool >> .wrongDeletion), PlaySound ErrorSound ]
        )


finishIfClean : Puzzle -> Session -> ( Model, List Effect ) -> ( Model, List Effect )
finishIfClean puzzle session ( model, effects ) =
    if Session.matches puzzle.cleanPoem session then
        ( { model
            | story =
                Story.conclude (Dialog.say Narrator (.pool >> .finale))
                    [ Dialog.say Mouse (.pool >> .mouse) ]
          }
        , effects ++ [ PlaySound PickupSound ]
        )

    else
        ( model, effects )


touchesJunk : Puzzle -> Buffer -> Bool
touchesJunk puzzle buffer =
    Core.wordUnderCursor buffer == puzzle.junk || List.member puzzle.junk (wordsIn (Core.selectedText buffer))


isSelection : Command -> Bool
isSelection command =
    List.member command [ SelectNextWordStart, SelectNextWordEnd, SelectPreviousWordStart, SelectLine ]


{-| The words of a text, without punctuation: "scale! splash" gives
["scale", "splash"].
-}
wordsIn : String -> List String
wordsIn text =
    String.words text
        |> List.map (String.filter (\c -> Char.isAlphaNum c || Char.toCode c > 127))
        |> List.filter (not << String.isEmpty)


junkLeft : Puzzle -> Buffer -> Int
junkLeft puzzle buffer =
    wordsIn (Core.toString buffer) |> List.filter ((==) puzzle.junk) |> List.length


floodGone : Puzzle -> Buffer -> Bool
floodGone puzzle buffer =
    not (List.member puzzle.flood (wordsIn (Core.toString buffer)))


score : Texts -> Model -> Contract.Score
score texts model =
    { tarts =
        if model.wrongDeletions == 0 then
            3

        else if model.wrongDeletions <= 2 then
            2

        else
            1
    , stats =
        [ { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes (currentWork texts model).session) }
        , { label = .results >> .wrongDeletions, value = \_ -> String.fromInt model.wrongDeletions }
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
            [ viewPool model.clock
            , Html.Lazy.lazy Editor.View.view buffer
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context.texts model work.puzzle buffer
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys work.session)
        , position = Editor.View.position buffer
        }
    , keyboard = { target = Nothing, helpers = [ "h", "j", "k", "l", "w", "e", "b", "x", "d", "u" ] }
    }


viewPanel : Texts -> Model -> Puzzle -> Buffer -> Html msg
viewPanel texts model puzzle buffer =
    let
        left =
            junkLeft puzzle buffer
    in
    Html.div [ A.class "panel-content" ]
        [ Html.p [ A.class "idea" ] [ Html.text texts.pool.idea ]
        , Html.div [ A.class "columns" ]
            [ Html.div [ A.class "column" ]
                [ Checklist.view
                    [ ( model.reachedJunk, texts.pool.walkToJunk )
                    , ( model.selectedSomething, texts.pool.pressW )
                    , ( model.deletedJunk, texts.pool.pressD )
                    , ( left == 0, texts.pool.removeAll left )
                    , ( floodGone puzzle buffer, texts.pool.removeLine )
                    ]
                , Html.p [ A.class "eyebrow" ] [ Html.text texts.pool.undoTip ]
                ]
            , Checklist.goal texts.pool.goal puzzle.cleanPoem
            ]
        ]


{-| Water, Alice swimming, the Mouse paddling about, tears falling.
-}
viewPool : Float -> Svg msg
viewPool clock =
    let
        seconds =
            clock / 1000

        -- Each wave is drawn once (`wavePath`) and slid sideways; sliding by
        -- one wavelength looks exactly like the start again.
        wave row =
            Svg.g [ SA.transform ("translate(" ++ String.fromFloat (negate (waveShift seconds)) ++ " 0)") ]
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
            sin (seconds * 2) * 2
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        (Svg.rect [ SA.width "320", SA.height "56", SA.fill "#1f3550" ] []
            :: List.map wave (List.range 0 4)
            ++ [ Sprite.viewAt { x = 120, y = 18 + bob, pixelSize = 2 } Sprites.aliceSwimming
               , Svg.rect [ SA.x "122", SA.y (String.fromFloat (34 + bob)), SA.width "20", SA.height "2", SA.fill "#6fa8dc" ] []
               , Sprite.viewAt { x = 200 + sin (seconds * 0.8) * 16, y = 30 + sin (seconds * 2.4) * 1.5, pixelSize = 2 } Sprites.mouse
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
