module Chapter.RabbitHole exposing (Model, Msg(..), groundDepth, init, jarSpeed, update, view)

{-| Chapter 1, "Down the Rabbit Hole": the home row. Alice falls down the
well; jars float up with a letter on their label, and pressing that letter
catches the jar. Six stages introduce the fingers pair by pair.

The fall: every jar caught brings Alice one step closer to the ground.
Every mistake, a wrong key or a jar that floats away, lifts her back up a
little. The closer the ground, the faster the jars. Before falling, the
player picks how deep the well is and how fast it gets: easy, challenge or
mad.
Esc lets go at any moment and Alice lands where she is, in a heap of sticks
and dry leaves.

Tarts follow the depth Alice landed at: the easy ground gives one, the
challenge ground two, and only the mad ground three. Letting go before the easy ground
earns none.

The stages come from the keyboard layout, so a Swiss child practises
`a s d f · j k l ö` and a French child `q s d f · j k l m`.

Jars appear at random places, but the randomness comes from a seed passed
to `init`: the same seed gives the same game, which is what tests use.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.RabbitHole.Well as Well
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
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
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


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


{-| The jars of all six stages together.
-}
basicJars : Int
basicJars =
    stages Layout.UsQwerty |> List.map .catches |> List.sum


{-| After the six stages, jars come with all the home-row keys.
-}
allKeysStage : Layout -> Stage
allKeysStage layout =
    let
        home =
            Layout.homeKeys layout
    in
    { keys = home.left ++ Layout.stretchKeys layout ++ home.right
    , catches = 0
    , describe = .rabbitHole >> .allKeys
    }



-- DIFFICULTY


{-| How each difficulty plays.

  - `ground`: how deep the well is, in jars. Easy ends with the basic jars,
    challenge adds a few seconds, mad nearly thirty seconds at full speed.
  - `topSpeed`: how fast jars rise near the ground, in pixels per second.
  - `shortestGap`: the pause between two jars near the ground, in ms.
  - `mostJars`: how many jars may be on screen at once near the ground.

Reaching a difficulty's ground is worth its tarts (`Difficulty.tarts`).

-}
type alias Pace =
    { ground : Int
    , topSpeed : Float
    , shortestGap : Float
    , mostJars : Int
    }


pace : Difficulty -> Pace
pace difficulty =
    case difficulty of
        Easy ->
            { ground = basicJars, topSpeed = 40, shortestGap = 900, mostJars = 4 }

        Challenge ->
            { ground = basicJars + 8, topSpeed = 60, shortestGap = 650, mostJars = 5 }

        Mad ->
            { ground = basicJars + 40, topSpeed = 95, shortestGap = 400, mostJars = 6 }


groundDepth : Difficulty -> Int
groundDepth difficulty =
    (pace difficulty).ground


{-| Tarts for landing at a depth: one for every ground Alice got down to,
counting only the grounds of the chosen difficulty and easier ones.
-}
tartsAt : Difficulty -> Int -> Int
tartsAt chosen depth =
    Difficulty.all
        |> List.filter (\d -> Difficulty.tarts d <= Difficulty.tarts chosen && depth >= (pace d).ground)
        |> List.length



-- TUNING
--
-- Everything that changes during the fall follows `nearness`: 0 at the top
-- of the well, 1 at the ground of the chosen difficulty.


{-| How fast jars rise at the top of the well, in pixels per second.
-}
startSpeed : Float
startSpeed =
    18


{-| The pause between two jars at the top of the well, in ms.
-}
longestGap : Float
longestGap =
    1500


fewestJars : Int
fewestJars =
    3


{-| How many jars' worth of depth a mistake costs.
-}
mistakeLift : Int
mistakeLift =
    3


nearness : Difficulty -> Int -> Float
nearness difficulty depth =
    clamp 0 1 (toFloat depth / toFloat (pace difficulty).ground)


between : Float -> Float -> Float -> Float
between from to fraction =
    from + (to - from) * fraction


{-| How fast jars rise at a depth, in pixels per millisecond.
-}
jarSpeed : Difficulty -> Int -> Float
jarSpeed difficulty depth =
    between startSpeed (pace difficulty).topSpeed (nearness difficulty depth) / 1000


spawnGap : Difficulty -> Int -> Float
spawnGap difficulty depth =
    between longestGap (pace difficulty).shortestGap (nearness difficulty depth)


jarsAtOnce : Difficulty -> Int -> Int
jarsAtOnce difficulty depth =
    fewestJars + round (nearness difficulty depth * toFloat ((pace difficulty).mostJars - fewestJars))


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


{-| Where Alice is drawn: at the top of the well before the first catch,
`lowestAliceY` just before the ground.
-}
topAliceY : Float
topAliceY =
    30


lowestAliceY : Float
lowestAliceY =
    96


aliceX : Float
aliceX =
    148


{-| The landing: Alice drops into the heap after `impactMilliseconds`, the
straw settles, and the closing story starts after `landingMilliseconds`.
-}
impactMilliseconds : Float
impactMilliseconds =
    550


landingMilliseconds : Float
landingMilliseconds =
    1700


{-| Alice's position resting in the heap, and the heap's top when it is
fully in view.
-}
restingAliceY : Float
restingAliceY =
    130


heapHeight : Float
heapHeight =
    44



-- MODEL


type alias Model =
    { story : Story
    , choice : Choice
    , difficulty : Maybe Difficulty
    , stage : Int
    , caughtThisStage : Int
    , depth : Int
    , jars : List Jar
    , flying : List FlyingJar
    , nextJarId : Int
    , hits : Int
    , errors : Int
    , streak : Int
    , bestStreak : Int
    , spawnIn : Float
    , seed : Random.Seed
    , lastLetter : Maybe Char
    , wobble : Float
    , aliceY : Float
    , landing : Maybe Landing
    , scroll : Float
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


{-| Alice let go (or reached the ground) at `depth`, from height `fromY`;
`age` is the time since then.
-}
type alias Landing =
    { depth : Int
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
    , choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , stage = 0
    , caughtThisStage = 0
    , depth = 0
    , jars = []
    , flying = []
    , nextJarId = 0
    , hits = 0
    , errors = 0
    , streak = 0
    , bestStreak = 0
    , spawnIn = 400
    , seed = seed
    , lastLetter = Nothing
    , wobble = 0
    , aliceY = topAliceY
    , landing = Nothing
    , scroll = 0
    , clock = 0
    }


{-| The stage being played; after the six stages, all keys.
-}
currentStage : Layout -> Model -> Stage
currentStage layout model =
    stageAt layout model.stage |> Maybe.withDefault (allKeysStage layout)


isBeyondStages : Layout -> Model -> Bool
isBeyondStages layout model =
    model.stage >= List.length (stages layout)


{-| The jar to catch next: the one that has floated highest.
-}
nextJar : List Jar -> Maybe Jar
nextJar jars =
    List.sortBy .y jars |> List.head


{-| Where Alice should be drawn for her depth (she glides there smoothly).
-}
targetAliceY : Model -> Float
targetAliceY model =
    case model.difficulty of
        Just difficulty ->
            between topAliceY lowestAliceY (nearness difficulty model.depth)

        Nothing ->
            topAliceY



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
                        case ( model.difficulty, model.landing ) of
                            ( Nothing, _ ) ->
                                choose context.layout key model

                            ( Just difficulty, Nothing ) ->
                                fallingKey context.layout difficulty key model

                            ( Just _, Just _ ) ->
                                ( model, [] )

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
            frame context.layout milliseconds model



-- CHOOSING THE DIFFICULTY


choose : Layout -> String -> Model -> ( Model, List Effect )
choose layout key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            startFalling layout difficulty model

        DifficultyChoice.Unchanged ->
            ( model, [] )


startFalling : Layout -> Difficulty -> Model -> ( Model, List Effect )
startFalling layout difficulty model =
    ( { model | difficulty = Just difficulty, spawnIn = 400 }
    , PlaySound PickupSound
        :: (stageAt layout 0 |> Maybe.map (\stage -> [ ShowToast stage.describe ]) |> Maybe.withDefault [])
    )



-- FALLING


fallingKey : Layout -> Difficulty -> String -> Model -> ( Model, List Effect )
fallingKey layout difficulty key model =
    if key == "Escape" then
        startLanding model

    else
        case String.toList (String.toLower key) of
            [ letter ] ->
                if letter == ' ' then
                    ( model, [] )

                else
                    case List.filter (\jar -> jar.letter == letter) model.jars |> nextJar of
                        Just jar ->
                            caught layout difficulty jar model

                        Nothing ->
                            ( mistake 1 model, [ PlaySound ErrorSound ] )

            _ ->
                ( model, [] )


{-| Mistakes lift Alice back up, never above the top of the well.
-}
mistake : Int -> Model -> Model
mistake count model =
    { model
        | errors = model.errors + count
        , streak = 0
        , wobble = wobbleMilliseconds
        , depth = max 0 (model.depth - count * mistakeLift)
    }


caught : Layout -> Difficulty -> Jar -> Model -> ( Model, List Effect )
caught layout difficulty jar model =
    let
        catcher =
            { model
                | jars = List.filter (\other -> other.id /= jar.id) model.jars
                , flying = { letter = jar.letter, fromX = jar.x, fromY = jar.y, age = 0 } :: model.flying
                , hits = model.hits + 1
                , streak = model.streak + 1
                , bestStreak = max model.bestStreak (model.streak + 1)
                , caughtThisStage = model.caughtThisStage + 1
                , depth = model.depth + 1
            }

        effects =
            [ PlaySound CorrectSound ]

        stageDone =
            not (isBeyondStages layout model)
                && (catcher.caughtThisStage >= (currentStage layout model).catches)
    in
    if catcher.depth >= groundDepth difficulty then
        startLanding catcher |> Tuple.mapSecond ((++) effects)

    else if stageDone then
        nextStage layout catcher effects

    else
        ( catcher, effects )


nextStage : Layout -> Model -> List Effect -> ( Model, List Effect )
nextStage layout model effects =
    let
        advanced =
            { model | stage = model.stage + 1, caughtThisStage = 0, jars = [], spawnIn = 800 }
    in
    ( advanced
    , effects ++ [ PlaySound PickupSound, ShowToast (currentStage layout advanced).describe ]
    )


startLanding : Model -> ( Model, List Effect )
startLanding model =
    ( { model | jars = [], landing = Just { depth = model.depth, fromY = model.aliceY, age = 0 } }
    , []
    )


frame : Layout -> Float -> Model -> ( Model, List Effect )
frame layout milliseconds model =
    let
        advanced =
            { model
                | clock = model.clock + milliseconds
                , wobble = max 0 (model.wobble - milliseconds)
                , aliceY = model.aliceY + (targetAliceY model - model.aliceY) * min 1 (milliseconds * 0.006)
                , scroll = model.scroll + milliseconds * scrollSpeed model
                , flying =
                    model.flying
                        |> List.map (\jar -> { jar | age = jar.age + milliseconds })
                        |> List.filter (\jar -> jar.age < flyMilliseconds)
                , story = Story.tick milliseconds model.story
            }
    in
    case ( advanced.landing, advanced.difficulty ) of
        ( Just landing, _ ) ->
            animateLanding milliseconds landing advanced

        ( Nothing, Just difficulty ) ->
            if Story.isPlaying advanced.story then
                advanced
                    |> spawnJar layout difficulty milliseconds
                    |> floatJars milliseconds

            else
                ( advanced, [] )

        ( Nothing, Nothing ) ->
            ( advanced, [] )


{-| How fast the walls slide past, in pixels per millisecond: faster the
deeper Alice is, still once she has landed.
-}
scrollSpeed : Model -> Float
scrollSpeed model =
    case ( model.landing, model.difficulty ) of
        ( Just landing, _ ) ->
            if landing.age < impactMilliseconds then
                0.12

            else
                0

        ( Nothing, Just difficulty ) ->
            between 0.026 0.08 (nearness difficulty model.depth)

        ( Nothing, Nothing ) ->
            0.026


floatJars : Float -> Model -> ( Model, List Effect )
floatJars milliseconds model =
    let
        ( stillVisible, missed ) =
            model.jars
                |> List.map (\jar -> { jar | y = jar.y - jar.speed * milliseconds })
                |> List.partition (\jar -> jar.y > jarGoneY)

        withVisible =
            { model | jars = stillVisible }
    in
    if List.isEmpty missed then
        ( withVisible, [] )

    else
        ( mistake (List.length missed) withVisible, [ PlaySound ErrorSound ] )


{-| A missed jar frees its place, so a stage always ends after its number of
catches, however many jars float away. After the stages there is no limit.
-}
spawnJar : Layout -> Difficulty -> Float -> Model -> Model
spawnJar layout difficulty milliseconds model =
    let
        countdown =
            model.spawnIn - milliseconds

        stage =
            currentStage layout model

        hasRoom =
            List.length model.jars
                < jarsAtOnce difficulty model.depth
                && (isBeyondStages layout model || model.caughtThisStage + List.length model.jars < stage.catches)
    in
    if countdown <= 0 && hasRoom then
        let
            ( jar, seed ) =
                Random.step (jarGenerator model.nextJarId (jarSpeed difficulty model.depth) stage.keys model.lastLetter) model.seed
        in
        { model
            | jars = model.jars ++ [ jar ]
            , seed = seed
            , nextJarId = model.nextJarId + 1
            , lastLetter = Just jar.letter
            , spawnIn = spawnGap difficulty model.depth
        }

    else
        { model | spawnIn = countdown }


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



-- LANDING


{-| Alice drops, hits the heap ("Thump!"), the straw settles, then the
closing story starts.
-}
animateLanding : Float -> Landing -> Model -> ( Model, List Effect )
animateLanding milliseconds landing model =
    let
        age =
            landing.age + milliseconds

        thump =
            if landing.age < impactMilliseconds && age >= impactMilliseconds then
                [ PlaySound BumpSound ]

            else
                []

        landed =
            { model | landing = Just { landing | age = age } }
    in
    if age >= landingMilliseconds && Story.isPlaying model.story then
        ( { landed
            | story =
                Story.conclude (Dialog.say Narrator (.rabbitHole >> .landing1))
                    [ Dialog.say Narrator (.rabbitHole >> .landing2) ]
          }
        , thump
        )

    else
        ( landed, thump )



-- SCORE


accuracy : Model -> Float
accuracy model =
    if model.hits + model.errors == 0 then
        1

    else
        toFloat model.hits / toFloat (model.hits + model.errors)


percent : Float -> String
percent fraction =
    String.fromInt (round (fraction * 100)) ++ "%"


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.rabbitHole.easyInfo

        Challenge ->
            texts.rabbitHole.challengeInfo

        Mad ->
            texts.rabbitHole.madInfo


score : Model -> Contract.Score
score model =
    let
        difficulty =
            Maybe.withDefault Easy model.difficulty

        landedAt =
            model.landing |> Maybe.map .depth |> Maybe.withDefault model.depth
    in
    { tarts = tartsAt difficulty landedAt
    , stats =
        [ { label = .results >> .difficulty, value = \texts -> Difficulty.name texts difficulty }
        , { label = .results >> .accuracy, value = \_ -> percent (accuracy model) }
        , { label = .results >> .jarsCaught, value = \_ -> String.fromInt model.hits }
        , { label = .results >> .bestStreak, value = \_ -> String.fromInt model.bestStreak }
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
        target =
            if model.landing == Nothing then
                nextJar model.jars

            else
                Nothing
    in
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Svg.svg [ SA.viewBox "0 0 320 192", SA.class "pixel-scene" ]
                (Well.background model.scroll
                    ++ List.map (\jar -> Well.jar { letter = jar.letter, x = jar.x, y = jar.y, size = 1, highlighted = Just jar == target }) model.jars
                    ++ List.map viewFlying model.flying
                    ++ viewAliceAndHeap model
                )
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.rabbitHole.chooseTitle
                    , hint = context.texts.rabbitHole.chooseHint
                    , info = difficultyInfo context.texts
                    }
                    model.choice

              else
                Html.text ""
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context model
    , status =
        { mode = "INS"
        , file = file
        , pendingKeys = ""
        , position =
            case model.difficulty of
                Just difficulty ->
                    String.fromInt model.depth ++ "/" ++ String.fromInt (groundDepth difficulty)

                Nothing ->
                    ""
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
        , x = jar.fromX + (aliceX - jar.fromX) * progress
        , y = jar.fromY + (jarTargetY - jar.fromY) * progress
        , size = 1 - progress * 0.5
        , highlighted = False
        }


jarTargetY : Float
jarTargetY =
    40


{-| Alice, and the heap that comes into view as the ground gets near. The
heap is drawn after Alice, so once she has landed she sits in the straw.
-}
viewAliceAndHeap : Model -> List (Svg.Svg msg)
viewAliceAndHeap model =
    let
        heapInView =
            case ( model.landing, model.difficulty ) of
                ( Just landing, Just difficulty ) ->
                    max (heapVisibility difficulty landing.depth) (min 1 (landing.age / 400))

                ( Nothing, Just difficulty ) ->
                    heapVisibility difficulty model.depth

                _ ->
                    0

        heapTop =
            192 - heapHeight * heapInView

        wobble =
            if model.wobble > 0 then
                sin (model.clock * 0.07) * 2

            else
                0

        aliceY =
            case model.landing of
                Just landing ->
                    if landing.age < impactMilliseconds then
                        let
                            t =
                                landing.age / impactMilliseconds
                        in
                        between landing.fromY restingAliceY (t * t)

                    else
                        restingAliceY

                Nothing ->
                    model.aliceY + sin (model.clock * 0.0022) * 3
    in
    Sprite.viewAt { x = aliceX + wobble, y = aliceY, pixelSize = 2 } Sprites.aliceFalling
        :: (if heapInView > 0 then
                [ Well.heap heapTop ]

            else
                []
           )
        ++ (case model.landing of
                Just landing ->
                    Well.strawBurst { x = aliceX + 12, y = 150, age = landing.age - impactMilliseconds }

                Nothing ->
                    []
           )


{-| The heap appears from below over the last part of the fall.
-}
heapVisibility : Difficulty -> Int -> Float
heapVisibility difficulty depth =
    clamp 0 1 ((nearness difficulty depth - 0.6) / 0.4)


viewPanel : Context -> Model -> Html msg
viewPanel context model =
    let
        texts =
            context.texts

        stat label value =
            Html.span [] [ Html.text label, Html.b [] [ Html.text value ] ]
    in
    case model.difficulty of
        Nothing ->
            Html.div [ A.class "panel-content" ]
                [ Html.p [ A.class "big" ] [ Html.text texts.rabbitHole.chooseTitle ]
                , Html.p [] [ Html.text texts.rabbitHole.chooseHint ]
                ]

        Just difficulty ->
            let
                stage =
                    currentStage context.layout model

                allStages =
                    stages context.layout

                stageLabel =
                    if isBeyondStages context.layout model then
                        texts.rabbitHole.allKeysLabel

                    else
                        texts.rabbitHole.stage (model.stage + 1) (List.length allStages)

                ground =
                    groundDepth difficulty
            in
            Html.div [ A.class "panel-content" ]
                [ Html.span [ A.class "eyebrow" ]
                    [ Html.text
                        (Difficulty.name texts difficulty
                            ++ " · "
                            ++ stageLabel
                            ++ " · "
                            ++ String.join " " (List.map upper stage.keys)
                        )
                    ]
                , Html.p [ A.class "big" ] [ Html.text (stage.describe texts) ]
                , Html.div [ A.class "meter" ]
                    [ Html.i [ A.style "width" (String.fromFloat (100 * nearness difficulty model.depth) ++ "%") ] [] ]
                , Html.div [ A.class "stats" ]
                    [ stat texts.rabbitHole.toGround (String.fromInt (max 0 (ground - model.depth)))
                    , stat texts.results.accuracy (percent (accuracy model))
                    , stat texts.results.bestStreak (String.fromInt model.bestStreak)
                    , stat texts.rabbitHole.speed (String.fromInt (round (jarSpeed difficulty model.depth * 1000)))
                    ]
                , Html.p [ A.class "eyebrow" ] [ Html.text texts.rabbitHole.letGo ]
                ]
