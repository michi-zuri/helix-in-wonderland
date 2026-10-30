module Chapter.Curiouser exposing (Currant, Model, Msg(..), init, lifetime, update, view)

{-| Chapter 4, "Curiouser and Curiouser": Helix's jumps that start with
`g`, played like whack-a-mole. The currant letters of the EAT ME cake hop
onto the crumbs on the table; the cursor reaching one squashes it.

Stages bring in one jump at a time, each with currants placed so that jump
is the quick way there (`Chapter.Curiouser.Table`): `gl` and `gh`, then
`gg` and `ge`, then `gj` and `gk` on long wrapped lines, then `g.`: cheeky currants
that run back to where the last one was squashed as soon as the cursor gets
onto their line or column. In the last stage every currant is cheeky.

`gw` works too, but nothing teaches it. A player who finishes the whole
chapter with nothing but `gw` and labels, every one a catch, meets the
Cheshire Cat at the end (`gwOnly`): an easter egg nothing announces.

The table is a Helix buffer with soft wrap; squashing really changes the
text (`Session.replaceChar`), which is what `g.` jumps back to. The
currants' letters, in order, spell a hidden sentence (`Texts.CuriouserTexts`).

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Curiouser.Table as Table exposing (Spot, Trick(..))
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
import Editor.Core as Core exposing (Command(..))
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
import Texts exposing (Texts)
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "04_eat_me.txt"



-- TUNING


{-| How long a currant waits to be squashed before it rolls away, in
milliseconds.
-}
lifetime : Difficulty -> Float
lifetime difficulty =
    case difficulty of
        Easy ->
            9000

        Challenge ->
            5500

        Mad ->
            3500


{-| In the last milliseconds of its life a currant blinks.
-}
hurryMilliseconds : Float
hurryMilliseconds =
    1500


{-| The pause before the next currant hops off the cake.
-}
gapMilliseconds : Float
gapMilliseconds =
    700


{-| How much of the chapter each stage takes, in order; the rest is the
final stage, where every currant is cheeky.
-}
stages : List ( Float, Trick )
stages =
    [ ( 0.2, LineEnds ), ( 0.4, FileEnds ), ( 0.6, RealLines ), ( 0.8, LastChange ) ]



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , session : Session
    , seed : Random.Seed
    , clock : Float
    , letters : List Char
    , nextLetter : Int
    , currants : List Currant
    , marks : List Mark
    , squashed : List Int
    , escaped : List Int
    , untilNext : Float
    , lastSquash : Maybe Int
    , gwOnly : Bool
    , gwStep : Int
    , cheshire : Bool
    }


{-| A currant on the table. `runsBack` currants run to the last squashed
spot once the cursor gets onto their line or column.
-}
type alias Currant =
    { index : Int
    , letter : Char
    , position : Int
    , age : Float
    , trick : Trick
    , runsBack : Bool
    }


{-| What the editor draws over the crumbs. Kept in the model and changed
only when a currant appears, moves, blinks or goes, so the editor is not
drawn again every animation frame.
-}
type alias Mark =
    { position : Int, text : Char, class : String }


init : Random.Seed -> Model
init seed =
    { story =
        Story.begin (Dialog.say Narrator (.curiouser >> .intro1))
            [ Dialog.say Alice (.curiouser >> .intro2)
            , Dialog.say Narrator (.curiouser >> .intro3)
            , Dialog.say Narrator (.curiouser >> .intro4)
            ]
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , session = Session.fromBuffer (Core.withWrap Table.wrapWidth (Core.fromString Table.text))
    , seed = seed
    , clock = 0
    , letters = []
    , nextLetter = 0
    , currants = []
    , marks = []
    , squashed = []
    , escaped = []
    , untilNext = gapMilliseconds
    , lastSquash = Nothing
    , gwOnly = True
    , gwStep = 0
    , cheshire = False
    }


lettersFor : Texts -> Difficulty -> List Char
lettersFor texts difficulty =
    (case difficulty of
        Easy ->
            texts.curiouser.easyLetters

        Challenge ->
            texts.curiouser.challengeLetters

        Mad ->
            texts.curiouser.madLetters
    )
        |> String.toList
        |> List.filter ((/=) ' ')


{-| The stage of the currant with this index: one trick being learned, or
`Nothing` for the last stage, where every trick comes up.
-}
stageOf : Model -> Int -> Maybe Trick
stageOf model index =
    let
        progress =
            toFloat index / toFloat (max 1 (List.length model.letters))
    in
    stages
        |> List.filter (\( until, _ ) -> progress < until)
        |> List.head
        |> Maybe.map Tuple.second


{-| How many currants can be on the table at once.
-}
atOnce : Model -> Int
atOnce model =
    if model.difficulty == Just Mad && stageOf model model.nextLetter == Nothing then
        2

    else
        1



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
                                jump key model

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
                    ( { model | story = story }, [ Finish (score model) ] )

        Frame milliseconds ->
            { model | clock = model.clock + milliseconds, story = Story.tick milliseconds model.story }
                |> flow milliseconds


choose : Texts -> String -> Model -> ( Model, List Effect )
choose texts key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            ( { model | difficulty = Just difficulty, letters = lettersFor texts difficulty }, [ PlaySound PickupSound ] )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| Moving, counts, and the `g` jumps. Selecting and editing come in the
next chapters.
-}
isUnlocked : Command -> Bool
isUnlocked command =
    case command of
        SelectRange _ ->
            True

        GotoLineNumber _ ->
            True

        _ ->
            List.member command
                [ MoveLeft
                , MoveRight
                , MoveDown
                , MoveUp
                , MoveLineDown
                , MoveLineUp
                , GotoLineStart
                , GotoLineEnd
                , GotoFirstNonWhitespace
                , GotoFileStart
                , GotoLastLine
                , GotoLastModification
                , GotoWord
                , CollapseSelection
                ]


jump : String -> Model -> ( Model, List Effect )
jump key model =
    let
        -- For the easter egg: is this key the next one of `g w` + label?
        fitsGw =
            case model.gwStep of
                0 ->
                    key == "g"

                1 ->
                    key == "w"

                _ ->
                    True
    in
    case Session.press isUnlocked key model.session of
        Session.Ran ran ->
            let
                ( squashed, effects ) =
                    squash { model | session = ran.session }

                caughtWithGw =
                    fitsGw && model.gwStep == 3 && List.length squashed.squashed > List.length model.squashed
            in
            ( { squashed | gwOnly = model.gwOnly && caughtWithGw, gwStep = 0 }, effects )
                |> andThen runBack

        Session.Waiting session ->
            ( { model | session = session, gwOnly = model.gwOnly && fitsGw && model.gwStep < 3, gwStep = model.gwStep + 1 }
            , []
            )

        Session.Locked session ->
            ( { model | session = session, gwOnly = False, gwStep = 0 }, [ ShowToast .notYet, PlaySound ErrorSound ] )

        Session.Ignored session ->
            ( { model | session = session, gwOnly = False, gwStep = 0 }, [] )


andThen : (Model -> ( Model, List Effect )) -> ( Model, List Effect ) -> ( Model, List Effect )
andThen next ( model, effects ) =
    next model |> Tuple.mapSecond ((++) effects)


cursorSpot : Model -> Spot
cursorSpot model =
    Table.spotOf (Core.head (Session.buffer model.session))


{-| Is the cursor on the currant, or the currant inside what `gw` selected?
-}
isCaught : Model -> Currant -> Bool
isCaught model currant =
    let
        ( first, last ) =
            Core.selectionRange (Session.buffer model.session)
    in
    currant.position >= first && currant.position <= last


{-| Squash every currant the cursor reached: the crumb under it becomes a
splat, a real change to the text, so `g.` goes back there.
-}
squash : Model -> ( Model, List Effect )
squash model =
    case List.filter (isCaught model) model.currants of
        [] ->
            ( model, [] )

        caught ->
            ( { model
                | currants = List.filter (not << isCaught model) model.currants
                , squashed = model.squashed ++ List.map .index caught
                , session = List.foldl (\currant -> Session.replaceChar currant.position '*') model.session caught
                , lastSquash = caught |> List.reverse |> List.head |> Maybe.map .position
              }
                |> withMarks
            , [ PlaySound CorrectSound ]
            )


{-| A shy, cheeky currant runs back to where the last one was squashed as
soon as the cursor gets onto its line or column, and waits there as long
as a new one would. Only one waits in that spot at a time.
-}
runBack : Model -> ( Model, List Effect )
runBack model =
    case model.lastSquash of
        Just hole ->
            let
                cursor =
                    cursorSpot model

                holeTaken =
                    List.any (\currant -> currant.position == hole) model.currants

                runs currant =
                    currant.runsBack && not holeTaken && Table.isInSight cursor (Table.spotOf currant.position)

                -- Only the first that wants to run goes; another waits its turn.
                runner =
                    model.currants |> List.filter runs |> List.head |> Maybe.map .index
            in
            if runner /= Nothing then
                ( { model
                    | currants =
                        List.map
                            (\currant ->
                                if Just currant.index == runner then
                                    { currant | position = hole, age = 0, runsBack = False }

                                else
                                    currant
                            )
                            model.currants
                  }
                    |> withMarks
                , [ PlaySound PortalSound ]
                )

            else
                ( model, [] )

        Nothing ->
            ( model, [] )


{-| Time passing: currants age and roll away, new ones hop off the cake,
and when the last letter is gone the closing story starts.
-}
flow : Float -> Model -> ( Model, List Effect )
flow milliseconds model =
    case model.difficulty of
        Just difficulty ->
            if Story.isPlaying model.story then
                let
                    aged =
                        List.map (\currant -> { currant | age = currant.age + milliseconds }) model.currants

                    ( gone, staying ) =
                        List.partition (\currant -> currant.age > lifetime difficulty) aged

                    startedHurrying =
                        List.any (\currant -> currant.age > lifetime difficulty - hurryMilliseconds && currant.age - milliseconds <= lifetime difficulty - hurryMilliseconds) staying

                    next =
                        { model
                            | currants = staying
                            , escaped = model.escaped ++ List.map .index gone
                            , untilNext = model.untilNext - milliseconds
                            , gwOnly = model.gwOnly && List.isEmpty gone
                        }

                    marked =
                        if List.isEmpty gone && not startedHurrying then
                            next

                        else
                            withMarks next

                    escapeEffects =
                        if List.isEmpty gone then
                            []

                        else
                            [ PlaySound ErrorSound, ShowToast (.curiouser >> .escaped) ]
                in
                spawn marked |> Tuple.mapSecond ((++) escapeEffects) |> andThen finishIfDone

            else
                ( model, [] )

        Nothing ->
            ( model, [] )


spawn : Model -> ( Model, List Effect )
spawn model =
    let
        letter =
            model.letters |> List.drop model.nextLetter |> List.head
    in
    case letter of
        Just char ->
            if model.untilNext <= 0 && List.length model.currants < atOnce model then
                let
                    stage =
                        stageOf model model.nextLetter

                    trick =
                        Maybe.withDefault LastChange stage

                    taken =
                        Core.head (Session.buffer model.session) :: List.map .position model.currants

                    ( spot, seed ) =
                        Random.step
                            (Table.place trick (cursorSpot model)
                                |> Random.andThen
                                    (\placed ->
                                        if List.member (Table.positionOf placed) taken then
                                            Table.placeAway (cursorSpot model)

                                        else
                                            Random.constant placed
                                    )
                            )
                            model.seed

                    currant =
                        { index = model.nextLetter
                        , letter = char
                        , position = Table.positionOf spot
                        , age = 0
                        , trick = trick
                        , runsBack = trick == LastChange && model.lastSquash /= Nothing
                        }

                    newStage =
                        model.nextLetter == 0 || stage /= stageOf model (model.nextLetter - 1)
                in
                ( { model
                    | currants = model.currants ++ [ currant ]
                    , nextLetter = model.nextLetter + 1
                    , untilNext = gapMilliseconds
                    , seed = seed
                  }
                    |> withMarks
                , PlaySound TickSound
                    :: (if newStage then
                            [ ShowToast (.curiouser >> .newTrick >> (\newTrick -> newTrick (keysOf stage))) ]

                        else
                            []
                       )
                )

            else
                ( model, [] )

        Nothing ->
            ( model, [] )


{-| The closing story; after a chapter of nothing but `gw`, every one a
catch, the Cheshire Cat turns up in it.
-}
finishIfDone : Model -> ( Model, List Effect )
finishIfDone model =
    if model.nextLetter >= List.length model.letters && List.isEmpty model.currants && not (List.isEmpty model.letters) && Story.isPlaying model.story then
        let
            cheshire =
                model.gwOnly && List.length model.squashed == List.length model.letters

            ending =
                if cheshire then
                    [ Dialog.say Alice (.curiouser >> .finale2)
                    , Dialog.say Narrator (.curiouser >> .cheshireAppears)
                    , Dialog.say CheshireCat (.curiouser >> .cheshireSays)
                    ]

                else
                    [ Dialog.say Alice (.curiouser >> .finale2) ]
        in
        ( { model | story = Story.conclude (Dialog.say Narrator (.curiouser >> .finale1)) ending, cheshire = cheshire }
        , if cheshire then
            [ PlaySound PickupSound, PlaySound PortalSound ]

          else
            [ PlaySound PickupSound ]
        )

    else
        ( model, [] )


withMarks : Model -> Model
withMarks model =
    let
        hurrying currant =
            currant.age > lifetime (Maybe.withDefault Easy model.difficulty) - hurryMilliseconds
    in
    { model
        | marks =
            List.map
                (\currant ->
                    { position = currant.position
                    , text = currant.letter
                    , class =
                        if hurrying currant then
                            "currant hurry"

                        else
                            "currant"
                    }
                )
                model.currants
    }


{-| The keys a stage teaches, for the toast.
-}
keysOf : Maybe Trick -> String
keysOf stage =
    case stage of
        Just LineEnds ->
            "gl gh"

        Just FileEnds ->
            "gg ge"

        Just RealLines ->
            "gj gk"

        Just LastChange ->
            "g."

        Nothing ->
            "g."


{-| The difficulty sets the most tarts there are (easy 1, challenge 2,
mad 3); currants that roll away can cost some of them.
-}
score : Model -> Contract.Score
score model =
    let
        difficulty =
            Maybe.withDefault Easy model.difficulty

        missed =
            List.length model.escaped

        forMisses =
            if missed <= 1 then
                3

            else if missed <= 3 then
                2

            else
                1
    in
    { tarts = min (Difficulty.tarts difficulty) forMisses
    , stats =
        [ { label = .results >> .difficulty, value = \texts -> Difficulty.name texts difficulty }
        , { label = .curiouser >> .squashed, value = \_ -> String.fromInt (List.length model.squashed) ++ "/" ++ String.fromInt (List.length model.letters) }
        , { label = .results >> .keystrokes, value = \_ -> String.fromInt (Session.keystrokes model.session) }
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
        buffer =
            Session.buffer model.session

        stage =
            stageOf model (max 0 (model.nextLetter - 1))
    in
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Html.Lazy.lazy2 viewCake context.texts.curiouser.cake (List.length model.squashed)
            , Html.Lazy.lazy2 viewTable model.session model.marks
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.curiouser.chooseTitle
                    , hint = context.texts.curiouser.chooseHint
                    , info = difficultyInfo context.texts
                    }
                    model.choice

              else
                Html.text ""
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context.texts model stage
    , status =
        { mode = Editor.View.modeName buffer
        , file = file
        , pendingKeys = String.join " " (Session.recentKeys model.session)
        , position = Editor.View.position buffer
        }
    }


viewTable : Session -> List Mark -> Html msg
viewTable session marks =
    Editor.View.viewWith { labels = Session.jumpLabels session, marks = marks } (Session.buffer session)


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.curiouser.easyInfo

        Challenge ->
            texts.curiouser.challengeInfo

        Mad ->
            texts.curiouser.madInfo


hintFor : Texts -> Maybe Trick -> String
hintFor texts stage =
    case stage of
        Just LineEnds ->
            texts.curiouser.lineEnds

        Just FileEnds ->
            texts.curiouser.fileEnds

        Just RealLines ->
            texts.curiouser.realLines

        Just LastChange ->
            texts.curiouser.lastChange

        Nothing ->
            texts.curiouser.allCheeky


viewPanel : Texts -> Model -> Maybe Trick -> Html msg
viewPanel texts model stage =
    case model.difficulty of
        Nothing ->
            Html.div [ A.class "panel-content" ]
                [ Html.p [ A.class "big" ] [ Html.text texts.curiouser.chooseTitle ]
                , Html.p [] [ Html.text texts.curiouser.chooseHint ]
                ]

        Just difficulty ->
            Html.div [ A.class "panel-content" ]
                [ Html.span [ A.class "eyebrow" ] [ Html.text (Difficulty.name texts difficulty) ]
                , Html.p [ A.class "big" ] [ Html.text (hintFor texts stage) ]
                , Html.div [ A.class "stats" ]
                    [ Html.span [] [ Html.text texts.curiouser.currants, Html.b [ A.class "currant-letters" ] [ Html.text (lettersSoFar model) ] ]
                    , Html.span [] [ Html.text texts.curiouser.squashed, Html.b [] [ Html.text (String.fromInt (List.length model.squashed) ++ "/" ++ String.fromInt (List.length model.letters)) ] ]
                    ]
                ]


{-| The letters squashed so far, a dot for each that rolled away, an
underscore for each still to come.
-}
lettersSoFar : Model -> String
lettersSoFar model =
    model.letters
        |> List.indexedMap
            (\index letter ->
                if List.member index model.squashed then
                    letter

                else if List.member index model.escaped then
                    '·'

                else
                    '_'
            )
        |> String.fromList


{-| The glass box with the little cake on the table, Alice beside it.
-}
viewCake : String -> Int -> Svg msg
viewCake label squashedCount =
    let
        labelWidth =
            toFloat (String.length label * 7 + 8)
    in
    Svg.svg [ SA.viewBox "0 0 320 56", SA.class "banner" ]
        [ Svg.rect [ SA.width "320", SA.height "56", SA.fill "#4a3060" ] []
        , Svg.rect [ SA.y "44", SA.width "320", SA.height "12", SA.fill "#5a3d73" ] []
        , Svg.rect [ SA.x "60", SA.y "20", SA.width "70", SA.height "24", SA.fill "#bfe6f0", SA.fillOpacity "0.25" ] []
        , Svg.rect [ SA.x "60", SA.y "20", SA.width "70", SA.height "1", SA.fill "#bfe6f0" ] []
        , Svg.rect [ SA.x "70", SA.y "32", SA.width "50", SA.height "12", SA.fill "#efba5d" ] []
        , Svg.rect [ SA.x "70", SA.y "30", SA.width "50", SA.height "3", SA.fill "#f4ead2" ] []
        , Svg.rect [ SA.x (String.fromFloat (95 - labelWidth / 2)), SA.y "6", SA.width (String.fromFloat labelWidth), SA.height "12", SA.fill "#f4ead2" ] []
        , Svg.text_ [ SA.x "95", SA.y "15", SA.class "bottle-label", SA.textAnchor "middle" ] [ Svg.text label ]
        , Svg.text_ [ SA.x "250", SA.y "28", SA.class "bottle-label", SA.textAnchor "middle" ] [ Svg.text (String.repeat (min 12 squashedCount) "•") ]
        , Sprite.viewAt { x = 200, y = 12, pixelSize = 2 } Sprites.alice
        ]
