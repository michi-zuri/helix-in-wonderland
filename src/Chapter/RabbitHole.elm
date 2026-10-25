module Chapter.RabbitHole exposing (Jar, Model, Msg(..), init, update, view)

{-| Chapter 1, "Down the Rabbit Hole": the home row. Alice falls past
shelves; jars float up with a letter on their label, and pressing that
letter catches the jar. Six stages introduce the fingers pair by pair.

Every catch makes the game a little faster. After the goal (40 jars) the
player may keep practising with all keys, faster and faster, and presses
Esc to land. The tarts are decided at the goal, so practising never costs
anything.

The stages come from the keyboard layout, so a Swiss child practises
`a s d f · j k l ö` and a French child `q s d f · j k l m`.

Jars appear at random places, but the randomness comes from a seed passed
to `init`: the same seed gives the same game, which is what tests use.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.RabbitHole.Well as Well
import Chapter.Story as Story exposing (Story)
import Html exposing (Html)
import Html.Attributes as A
import Keyboard.Layout as Layout exposing (Layout)
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Random
import Speaker exposing (Speaker(..))
import Svg
import Svg.Attributes as SA
import Texts exposing (Texts)
import Ui.Dialog as Dialog


file : String
file =
    "01_rabbit_hole.txt"



-- STAGES


type alias Stage =
    { keys : List Char
    , catches : Int
    , describe : Texts -> String
    }


{-| Index fingers, middle, ring, little fingers, all eight, then the index
fingers stretching inwards for g and h.
-}
stages : Layout -> List Stage
stages layout =
    let
        home =
            Layout.homeKeys layout

        nth index list =
            list |> List.drop index |> List.head |> Maybe.withDefault ' '

        -- Pair 0 is the index fingers (f, j), pair 3 the little fingers.
        pair index =
            ( nth (3 - index) home.left, nth index home.right )

        fingerStage index describe =
            let
                ( left, right ) =
                    pair index
            in
            { keys = [ left, right ]
            , catches = 6
            , describe = \texts -> describe texts (upper left) (upper right)
            }

        ( stretchLeft, stretchRight ) =
            case Layout.stretchKeys layout of
                [ g, h ] ->
                    ( g, h )

                _ ->
                    ( 'g', 'h' )
    in
    [ fingerStage 0 (.rabbitHole >> .indexFingers)
    , fingerStage 1 (.rabbitHole >> .middleFingers)
    , fingerStage 2 (.rabbitHole >> .ringFingers)
    , fingerStage 3 (.rabbitHole >> .littleFingers)
    , { keys = home.left ++ home.right, catches = 10, describe = .rabbitHole >> .allEight }
    , { keys = [ stretchLeft, stretchRight ]
      , catches = 6
      , describe = \texts -> texts.rabbitHole.stretch (upper stretchLeft) (upper stretchRight)
      }
    ]


upper : Char -> String
upper char =
    String.toUpper (String.fromChar char)


stageAt : Layout -> Int -> Maybe Stage
stageAt layout index =
    stages layout |> List.drop index |> List.head


{-| The jars needed to reach the goal.
-}
totalCatches : Int
totalCatches =
    stages Layout.UsQwerty |> List.map .catches |> List.sum



-- TUNING
-- The pace depends only on how many jars have been caught so far, so the game
-- speeds up with every catch, in practice too, without limit.


{-| How fast jars rise, in pixels per millisecond: about 5 seconds to cross
the screen at the start, about 2.5 at the goal, and quicker after that.
-}
jarSpeed : Int -> Float
jarSpeed catches =
    (18 + 0.7 * toFloat catches) / 1000


{-| The pause between two jars: 1.5 seconds at first, shrinking with every
catch, but never below 0.3 seconds.
-}
spawnMilliseconds : Int -> Float
spawnMilliseconds catches =
    max 300 (1500 * (0.97 ^ toFloat catches))


{-| Three jars at once to begin with, one more every 25 catches, at most six.
-}
maxJarsAtOnce : Int -> Int
maxJarsAtOnce catches =
    min 6 (3 + catches // 25)


jarStartY : Float
jarStartY =
    196


{-| A jar that floats above this line was missed.
-}
jarGoneY : Float
jarGoneY =
    -24


flyMilliseconds : Float
flyMilliseconds =
    350


wobbleMilliseconds : Float
wobbleMilliseconds =
    350


fallPixelsPerMillisecond : Float
fallPixelsPerMillisecond =
    0.042


alicePosition : { x : Float, y : Float }
alicePosition =
    { x = 148, y = 30 }



-- MODEL


type alias Model =
    { story : Story
    , stage : Int
    , caughtThisStage : Int
    , jars : List Jar
    , flying : List FlyingJar
    , nextJarId : Int
    , hits : Int
    , errors : Int
    , practising : Bool
    , practiceCatches : Int
    , accuracyAtGoal : Maybe Float
    , streak : Int
    , bestStreak : Int
    , spawnIn : Float
    , seed : Random.Seed
    , lastLetter : Maybe Char
    , wobble : Float
    , fallen : Float
    , clock : Float
    }


{-| A jar floating upwards. `speed` is in pixels per millisecond.
-}
type alias Jar =
    { id : Int
    , letter : Char
    , x : Float
    , y : Float
    , speed : Float
    }


{-| A caught jar on its way into Alice's hands.
-}
type alias FlyingJar =
    { letter : Char
    , fromX : Float
    , fromY : Float
    , age : Float
    }


init : Random.Seed -> Model
init seed =
    { story =
        Story.begin (Dialog.say Narrator (.rabbitHole >> .intro1))
            [ Dialog.say WhiteRabbit (.rabbitHole >> .intro2)
            , Dialog.say Narrator (.rabbitHole >> .intro3)
            , Dialog.say Narrator (.rabbitHole >> .intro4)
            ]
    , stage = 0
    , caughtThisStage = 0
    , jars = []
    , flying = []
    , nextJarId = 0
    , hits = 0
    , errors = 0
    , practising = False
    , practiceCatches = 0
    , accuracyAtGoal = Nothing
    , streak = 0
    , bestStreak = 0
    , spawnIn = 400
    , seed = seed
    , lastLetter = Nothing
    , wobble = 0
    , fallen = 0
    , clock = 0
    }


{-| The stage being played; after the goal, the practice round with all keys.
-}
currentStage : Layout -> Model -> Maybe Stage
currentStage layout model =
    if model.practising then
        Just (practiceStage layout)

    else
        stageAt layout model.stage


practiceStage : Layout -> Stage
practiceStage layout =
    let
        home =
            Layout.homeKeys layout
    in
    { keys = home.left ++ Layout.stretchKeys layout ++ home.right
    , catches = 0
    , describe = .rabbitHole >> .practice
    }


{-| The jar to catch next: the one that has floated highest.
-}
nextJar : List Jar -> Maybe Jar
nextJar jars =
    List.sortBy .y jars |> List.head



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
                        catch context.layout key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    ( { model | story = story }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story }
                    , stageAt context.layout 0 |> Maybe.map (\stage -> [ ShowToast stage.describe ]) |> Maybe.withDefault []
                    )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score model) ] )

        Frame milliseconds ->
            let
                advanced =
                    { model
                        | clock = model.clock + milliseconds
                        , fallen = model.fallen + milliseconds * fallPixelsPerMillisecond
                        , wobble = max 0 (model.wobble - milliseconds)
                        , flying =
                            model.flying
                                |> List.map (\jar -> { jar | age = jar.age + milliseconds })
                                |> List.filter (\jar -> jar.age < flyMilliseconds)
                        , story = Story.tick milliseconds model.story
                    }
            in
            if Story.isPlaying advanced.story then
                ( advanced |> floatJars milliseconds |> spawnJar context.layout milliseconds, [] )

            else
                ( advanced, [] )


floatJars : Float -> Model -> Model
floatJars milliseconds model =
    let
        ( stillVisible, missed ) =
            model.jars
                |> List.map (\jar -> { jar | y = jar.y - jar.speed * milliseconds })
                |> List.partition (\jar -> jar.y > jarGoneY)
    in
    { model
        | jars = stillVisible
        , streak =
            if List.isEmpty missed then
                model.streak

            else
                0
    }


{-| A missed jar frees its place, so a stage always ends after its number of
catches, however many jars float away. Practice has no end.
-}
spawnJar : Layout -> Float -> Model -> Model
spawnJar layout milliseconds model =
    let
        countdown =
            model.spawnIn - milliseconds
    in
    case currentStage layout model of
        Just stage ->
            let
                hasRoom =
                    List.length model.jars
                        < maxJarsAtOnce model.hits
                        && (model.practising || model.caughtThisStage + List.length model.jars < stage.catches)
            in
            if countdown <= 0 && hasRoom then
                let
                    ( jar, seed ) =
                        Random.step (jarGenerator model.nextJarId (jarSpeed model.hits) stage.keys model.lastLetter) model.seed
                in
                { model
                    | jars = model.jars ++ [ jar ]
                    , seed = seed
                    , nextJarId = model.nextJarId + 1
                    , lastLetter = Just jar.letter
                    , spawnIn = spawnMilliseconds model.hits
                }

            else
                { model | spawnIn = countdown }

        Nothing ->
            model


{-| A new jar at the bottom, at a random place, rising at about `speed`.
-}
jarGenerator : Int -> Float -> List Char -> Maybe Char -> Random.Generator Jar
jarGenerator id speed keys lastLetter =
    Random.map3
        (\letter x variation ->
            { id = id
            , letter = letter
            , x = x
            , y = jarStartY
            , speed = speed * variation
            }
        )
        (letterGenerator keys lastLetter)
        (Random.float 72 222)
        (Random.float 1 1.15)


{-| Usually a different letter than last time, so the same finger isn't
asked twice in a row too often.
-}
letterGenerator : List Char -> Maybe Char -> Random.Generator Char
letterGenerator keys lastLetter =
    let
        others =
            List.filter (\key -> Just key /= lastLetter) keys
    in
    case ( keys, others ) of
        ( first :: rest, other :: moreOthers ) ->
            Random.float 0 1
                |> Random.andThen
                    (\roll ->
                        if roll < 0.7 then
                            Random.uniform other moreOthers

                        else
                            Random.uniform first rest
                    )

        ( first :: rest, [] ) ->
            Random.uniform first rest

        ( [], _ ) ->
            Random.constant 'f'


catch : Layout -> String -> Model -> ( Model, List Effect )
catch layout key model =
    if key == "Escape" && model.practising then
        land model

    else
        catchLetter layout key model


catchLetter : Layout -> String -> Model -> ( Model, List Effect )
catchLetter layout key model =
    case String.toList (String.toLower key) of
        [ letter ] ->
            if letter == ' ' then
                ( model, [] )

            else
                case List.filter (\jar -> jar.letter == letter) model.jars |> nextJar of
                    Just jar ->
                        caught layout jar model

                    Nothing ->
                        ( { model | errors = model.errors + 1, streak = 0, wobble = wobbleMilliseconds }
                        , [ PlaySound ErrorSound, FlashKey (String.fromChar letter) False ]
                        )

        _ ->
            ( model, [] )


caught : Layout -> Jar -> Model -> ( Model, List Effect )
caught layout jar model =
    let
        catcher =
            { model
                | jars = List.filter (\other -> other.id /= jar.id) model.jars
                , flying = { letter = jar.letter, fromX = jar.x, fromY = jar.y, age = 0 } :: model.flying
                , hits = model.hits + 1
                , streak = model.streak + 1
                , bestStreak = max model.bestStreak (model.streak + 1)
                , caughtThisStage = model.caughtThisStage + 1
            }

        effects =
            [ PlaySound CorrectSound, FlashKey (String.fromChar jar.letter) True ]

        stageDone =
            not model.practising
                && (stageAt layout model.stage
                        |> Maybe.map (\stage -> catcher.caughtThisStage >= stage.catches)
                        |> Maybe.withDefault False
                   )
    in
    if model.practising then
        ( { catcher | practiceCatches = model.practiceCatches + 1 }, effects )

    else if stageDone then
        nextStage layout catcher effects

    else
        ( catcher, effects )


nextStage : Layout -> Model -> List Effect -> ( Model, List Effect )
nextStage layout model effects =
    case stageAt layout (model.stage + 1) of
        Just stage ->
            ( { model | stage = model.stage + 1, caughtThisStage = 0, jars = [], spawnIn = 800 }
            , effects ++ [ PlaySound PickupSound, ShowToast stage.describe ]
            )

        Nothing ->
            -- The goal: the score is fixed now, and practice begins.
            ( { model
                | practising = True
                , accuracyAtGoal = Just (accuracy model)
                , jars = []
                , spawnIn = 800
              }
            , effects ++ [ PlaySound PickupSound, ShowToast (.rabbitHole >> .goalReached) ]
            )


land : Model -> ( Model, List Effect )
land model =
    ( { model
        | jars = []
        , practising = False
        , story =
            Story.conclude (Dialog.say Narrator (.rabbitHole >> .landing1))
                [ Dialog.say Narrator (.rabbitHole >> .landing2) ]
      }
    , [ PlaySound PickupSound ]
    )


accuracy : Model -> Float
accuracy model =
    if model.hits + model.errors == 0 then
        1

    else
        toFloat model.hits / toFloat (model.hits + model.errors)


percent : Float -> String
percent fraction =
    String.fromInt (round (fraction * 100)) ++ "%"


{-| Jars caught towards the goal (practice jars not included).
-}
caughtSoFar : Layout -> Model -> Int
caughtSoFar layout model =
    if model.accuracyAtGoal /= Nothing then
        totalCatches

    else
        (stages layout |> List.take model.stage |> List.map .catches |> List.sum) + model.caughtThisStage


score : Model -> Contract.Score
score model =
    let
        fraction =
            Maybe.withDefault (accuracy model) model.accuracyAtGoal
    in
    { tarts =
        if fraction >= 0.95 then
            3

        else if fraction >= 0.85 then
            2

        else
            1
    , stats =
        [ { label = .results >> .accuracy, value = \_ -> percent fraction }
        , { label = .results >> .jarsCaught, value = \_ -> String.fromInt model.hits }
        , { label = .results >> .bestStreak, value = \_ -> String.fromInt model.bestStreak }
        , { label = .results >> .practiceJars, value = \_ -> String.fromInt model.practiceCatches }
        ]
    }



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    let
        target =
            nextJar model.jars

        stage =
            currentStage context.layout model

        practiceKeys =
            if model.practising then
                [ "Escape" ]

            else
                []
    in
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Svg.svg [ SA.viewBox "0 0 320 192", SA.class "pixel-scene" ]
                (Well.background model.fallen
                    ++ List.map (\jar -> Well.jar { letter = jar.letter, x = jar.x, y = jar.y, size = 1, highlighted = Just jar == target }) model.jars
                    ++ List.map viewFlying model.flying
                    ++ [ viewAlice model ]
                )
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context model
    , status =
        { mode = "INS"
        , file = file
        , pendingKeys = ""
        , position =
            String.fromInt (caughtSoFar context.layout model)
                ++ "/"
                ++ String.fromInt totalCatches
                ++ (if model.practising then
                        " +" ++ String.fromInt model.practiceCatches

                    else
                        ""
                   )
        }
    , keyboard =
        { target = Maybe.map .letter target
        , helpers = (stage |> Maybe.map (.keys >> List.map String.fromChar) |> Maybe.withDefault []) ++ practiceKeys
        }
    }


viewFlying : FlyingJar -> Svg.Svg msg
viewFlying jar =
    let
        progress =
            jar.age / flyMilliseconds
    in
    Well.jar
        { letter = jar.letter
        , x = jar.fromX + (alicePosition.x - jar.fromX) * progress
        , y = jar.fromY + (alicePosition.y + 10 - jar.fromY) * progress
        , size = 1 - progress * 0.5
        , highlighted = False
        }


viewAlice : Model -> Svg.Svg msg
viewAlice model =
    let
        wobble =
            if model.wobble > 0 then
                sin (model.clock * 0.07) * 2

            else
                0
    in
    Sprite.viewAt
        { x = alicePosition.x + wobble
        , y = alicePosition.y + sin (model.clock * 0.0022) * 3
        , pixelSize = 2
        }
        Sprites.aliceFalling


viewPanel : Context -> Model -> Html msg
viewPanel context model =
    let
        texts =
            context.texts

        allStages =
            stages context.layout

        current =
            min model.stage (List.length allStages - 1)

        caughtCount =
            caughtSoFar context.layout model

        stat label value =
            Html.span [] [ Html.text label, Html.b [] [ Html.text value ] ]
    in
    if model.practising then
        Html.div [ A.class "panel-content" ]
            [ Html.span [ A.class "eyebrow" ] [ Html.text texts.rabbitHole.practiceLabel ]
            , Html.p [ A.class "big" ] [ Html.text texts.rabbitHole.practice ]
            , Html.div [ A.class "stats" ]
                [ stat texts.results.practiceJars (String.fromInt model.practiceCatches)
                , stat texts.results.bestStreak (String.fromInt model.bestStreak)
                , stat texts.rabbitHole.speed (String.fromInt (round (jarSpeed model.hits * 1000)))
                ]
            ]

    else
        viewStagePanel texts model current allStages caughtCount stat


viewStagePanel : Texts -> Model -> Int -> List Stage -> Int -> (String -> String -> Html msg) -> Html msg
viewStagePanel texts model current allStages caughtCount stat =
    case allStages |> List.drop current |> List.head of
        Just stage ->
            Html.div [ A.class "panel-content" ]
                [ Html.span [ A.class "eyebrow" ]
                    [ Html.text
                        (texts.rabbitHole.stage (current + 1) (List.length allStages)
                            ++ " · "
                            ++ String.join " " (List.map upper stage.keys)
                        )
                    ]
                , Html.p [ A.class "big" ] [ Html.text (stage.describe texts) ]
                , Html.div [ A.class "meter" ]
                    [ Html.i [ A.style "width" (String.fromFloat (100 * toFloat caughtCount / toFloat totalCatches) ++ "%") ] [] ]
                , Html.div [ A.class "stats" ]
                    [ stat texts.results.jarsCaught (String.fromInt caughtCount ++ "/" ++ String.fromInt totalCatches)
                    , stat texts.results.accuracy (percent (accuracy model))
                    , stat texts.results.bestStreak (String.fromInt model.bestStreak)
                    ]
                ]

        Nothing ->
            Html.text ""
