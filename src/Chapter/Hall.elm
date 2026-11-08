module Chapter.Hall exposing (Model, Msg(..), backdrop, init, update, view)

{-| Chapter 2, "The Hall of Doors": Alice walks with `h j k l`, finds the
golden key, then learns counts (`8l`) on the way to the little curtain.

Three halls to choose from (see `Chapter.Hall.Map`): easy is open,
challenge has things dropped all over the floor, and in mad heaps of them
fill the hall behind the key, with a few free stepping stones. A count
jumps over dropped things (`Chapter.Hall.Move`); mad allows counts up to 5.
Every door is locked. Finishing is worth the difficulty's tarts.

Read top to bottom: the map, the tunable numbers, the model, `update`, then
the view. Everything here is pure; sounds, toasts and the end of the chapter
are requested as `Effect`s for `Main` to carry out.

-}

import Array
import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Hall.Map as Map exposing (Direction(..), Map, Point)
import Chapter.Hall.Move as Move
import Chapter.Hall.Route as Route
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Chapter.Story as Story exposing (Story)
import Difficulty exposing (Difficulty(..))
import Html
import Html.Attributes as HA
import Keyboard.Layout as Layout exposing (Layout)
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy
import Texts exposing (Texts)
import Ui.Dialog as Dialog
import Ui.DifficultyChoice as DifficultyChoice exposing (Choice)


file : String
file =
    "02_hall_of_doors.txt"



-- TUNING


stepMilliseconds : Float
stepMilliseconds =
    75


bumpMilliseconds : Float
bumpMilliseconds =
    150


{-| `99j` would take a while; counts are capped.
-}
largestCount : Int
largestCount =
    30


{-| The mad hall allows counts up to 5, so no jump crosses the whole hall
(#89).
-}
countLimit : Maybe Difficulty -> Int
countLimit difficulty =
    if difficulty == Just Mad then
        5

    else
        largestCount


{-| Keycap hints around Alice disappear after this many steps.
-}
stepsWithHints : Int
stepsWithHints =
    14


rabbitPixelsPerMillisecond : Float
rabbitPixelsPerMillisecond =
    0.07



-- MODEL


type alias Model =
    { choice : Choice
    , difficulty : Maybe Difficulty
    , fewestKeystrokes : Int
    , map : Map
    , alice : Point
    , step : Maybe Step
    , queue : List Action
    , pendingCount : String
    , keystrokes : Int
    , usedCount : Bool
    , hasKey : Bool
    , stepsTaken : Int
    , rabbit : Rabbit
    , story : Story
    , bumpRemaining : Float
    , clock : Float
    }


{-| What Alice will do next, planned when a move is typed
(`Chapter.Hall.Move`): step onto a tile, then maybe take the key, knock at
a locked door or bump into something.
-}
type Action
    = StepTo Point
    | TakeKeyAt Point
    | KnockAt
    | Bump


{-| The step Alice is animating: she already stands on the new tile in the
model, and is drawn sliding from `from` towards it.
-}
type alias Step =
    { from : Point
    , progress : Float
    }


{-| The rabbit hurries off along its route once the intro story ends.
Its position is in pixels, not tiles, so it can move smoothly.
-}
type alias Rabbit =
    { x : Float
    , y : Float
    , route : List Point
    , running : Bool
    }


{-| Chapters get a seed; nothing in this one is left to chance.
-}
init : Model
init =
    { choice = DifficultyChoice.start 0
    , difficulty = Nothing
    , fewestKeystrokes = 0
    , map = startingMap
    , alice = startOf startingMap
    , step = Nothing
    , queue = []
    , pendingCount = ""
    , keystrokes = 0
    , usedCount = False
    , hasKey = False
    , stepsTaken = 0
    , rabbit = { x = 12 * 16, y = 16, route = [ { x = 13, y = 1 }, { x = 14, y = 1 }, { x = 14, y = 0 } ], running = False }
    , story =
        Story.begin (Dialog.say Narrator (.hall >> .intro1))
            [ Dialog.say WhiteRabbit (.hall >> .intro2)
            , Dialog.say Narrator (.hall >> .intro3)
            , Dialog.say Narrator (.hall >> .intro4)
            ]
    , bumpRemaining = 0
    , clock = 0
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
                        case model.difficulty of
                            Just _ ->
                                explore key model

                            Nothing ->
                                choose key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    ( { model | story = story }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story, rabbit = startRunning model.rabbit, choice = DifficultyChoice.start model.clock }, [] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score model) ] )

        Frame milliseconds ->
            { model
                | clock = model.clock + milliseconds
                , bumpRemaining = max 0 (model.bumpRemaining - milliseconds)
                , rabbit = moveRabbit milliseconds model.rabbit
                , story = Story.tick milliseconds model.story
            }
                |> animateWalking context.layout milliseconds


choose : String -> Model -> ( Model, List Effect )
choose key model =
    case DifficultyChoice.press model.clock key model.choice of
        DifficultyChoice.Moved choice ->
            ( { model | choice = choice }, [ PlaySound TickSound ] )

        DifficultyChoice.Picked difficulty ->
            ( enterHall difficulty model, [ PlaySound PickupSound ] )

        DifficultyChoice.Unchanged ->
            ( model, [] )


{-| Set up the hall of the chosen difficulty.
-}
enterHall : Difficulty -> Model -> Model
enterHall difficulty model =
    let
        map =
            if difficulty == Easy then
                startingMap

            else
                Map.parse (Map.plan difficulty)
    in
    { model
        | difficulty = Just difficulty
        , map = map
        , alice = startOf map
        , fewestKeystrokes =
            Route.fewest { largestCount = countLimit (Just difficulty) }
                { at = startOf map, hasKey = False }
                map
                |> Maybe.map .keystrokes
                |> Maybe.withDefault 0
    }


startOf : Map -> Point
startOf map =
    Map.find Start map |> Maybe.withDefault { x = 1, y = 10 }


explore : String -> Model -> ( Model, List Effect )
explore key model =
    if String.startsWith "Arrow" key then
        ( model, [ ShowToast (.hall >> .arrows), PlaySound ErrorSound ] )

    else if isCountDigit key model.pendingCount then
        if (String.toInt (model.pendingCount ++ key) |> Maybe.withDefault 0) > countLimit model.difficulty then
            ( model, [ ShowToast (.hall >> .mostFive), PlaySound ErrorSound ] )

        else
            ( { model | pendingCount = model.pendingCount ++ key, keystrokes = model.keystrokes + 1 }
            , [ PlaySound TickSound ]
            )

    else if key == "Escape" then
        ( { model | pendingCount = "" }, [] )

    else
        case Map.directionFor key of
            Just direction ->
                walk direction model

            Nothing ->
                ( model, [] )


{-| A digit starts or extends a count, but a count can't start with 0.
-}
isCountDigit : String -> String -> Bool
isCountDigit key pendingCount =
    String.length key == 1 && String.all Char.isDigit key && not (key == "0" && pendingCount == "")


{-| A move is planned at once, from where Alice will stand once the moves
already typed are done (`Chapter.Hall.Move`), and then played step by step.
A move that fails as a whole (it would end on something dropped) is a bump.
-}
walk : Direction -> Model -> ( Model, List Effect )
walk direction model =
    let
        count =
            String.toInt model.pendingCount
                |> Maybe.withDefault 1
                |> clamp 1 (countLimit model.difficulty)

        ( from, hasKey ) =
            List.foldl
                (\action ( at, key ) ->
                    case action of
                        StepTo point ->
                            ( point, key )

                        TakeKeyAt _ ->
                            ( at, True )

                        _ ->
                            ( at, key )
                )
                ( model.alice, model.hasKey )
                model.queue

        actions =
            case Move.resolve direction count from model.map of
                Just move ->
                    List.map StepTo move.path
                        ++ (case move.ending of
                                Move.Stops ->
                                    []

                                Move.TakesKey keyAt ->
                                    if hasKey then
                                        [ Bump ]

                                    else
                                        [ TakeKeyAt keyAt ]

                                Move.KnocksAt ->
                                    [ KnockAt ]

                                Move.Bumps ->
                                    [ Bump ]
                           )

                Nothing ->
                    [ Bump ]
    in
    ( { model
        | queue = model.queue ++ actions
        , pendingCount = ""
        , keystrokes = model.keystrokes + 1
        , usedCount = model.usedCount || count > 1
      }
    , []
    )


animateWalking : Layout -> Float -> Model -> ( Model, List Effect )
animateWalking layout milliseconds model =
    case model.step of
        Just step ->
            let
                progress =
                    step.progress + milliseconds / stepMilliseconds
            in
            if progress >= 1 then
                arrive { model | step = Nothing }

            else
                ( { model | step = Just { step | progress = progress } }, [] )

        Nothing ->
            if Story.isTalking model.story then
                ( model, [] )

            else
                startNextAction layout model


startNextAction : Layout -> Model -> ( Model, List Effect )
startNextAction layout model =
    case model.queue of
        (StepTo target) :: rest ->
            ( { model
                | alice = target
                , step = Just { from = model.alice, progress = 0 }
                , queue = rest
                , stepsTaken = model.stepsTaken + 1
              }
            , [ PlaySound StepSound ]
            )

        (TakeKeyAt keyAt) :: _ ->
            takeKey layout keyAt model

        KnockAt :: _ ->
            ( { model
                | queue = []
                , bumpRemaining = bumpMilliseconds
                , story =
                    Story.tell
                        (if model.hasKey then
                            Dialog.say Narrator (.hall >> .keyTooSmall)

                         else
                            Dialog.say Narrator (.hall >> .doorLocked)
                        )
                        []
                        model.story
              }
            , [ PlaySound BumpSound ]
            )

        Bump :: _ ->
            ( { model | queue = [], bumpRemaining = bumpMilliseconds }, [ PlaySound BumpSound ] )

        [] ->
            ( model, [] )


{-| The key lies on the glass table (#86): walking into it, Alice takes it
and stays in front of the table. Any steps still queued are dropped. The
count lesson that follows also tells how to jump (#89).
-}
takeKey : Layout -> Point -> Model -> ( Model, List Effect )
takeKey layout keyAt model =
    ( { model
        | hasKey = True
        , map = Map.setTile keyAt GlassTable model.map
        , queue = []
        , story =
            Story.tell (Dialog.say Alice (.hall >> .keyFound))
                (Dialog.say Narrator (.hall >> .countLesson)
                    :: (if model.difficulty /= Just Easy then
                            [ Dialog.say Narrator (.hall >> .jumpLesson) ]

                        else
                            []
                       )
                    ++ (if Layout.needsShiftForDigits layout then
                            [ Dialog.say Narrator (.hall >> .azertyTip) ]

                        else
                            []
                       )
                )
                model.story
      }
    , [ PlaySound PickupSound ]
    )


{-| What happens when Alice finishes a step onto a tile.
-}
arrive : Model -> ( Model, List Effect )
arrive model =
    case Map.tileAt model.alice model.map of
        CurtainFront ->
            if model.hasKey then
                ( { model
                    | queue = []
                    , story = Story.conclude (Dialog.say Narrator (.hall >> .finale)) []
                  }
                , [ PlaySound PickupSound ]
                )

            else
                ( { model | queue = [] }, [ ShowToast (.hall >> .locked) ] )

        _ ->
            ( model, [] )


startRunning : Rabbit -> Rabbit
startRunning rabbit =
    { rabbit | running = True }


moveRabbit : Float -> Rabbit -> Rabbit
moveRabbit milliseconds rabbit =
    case ( rabbit.running, rabbit.route ) of
        ( True, target :: rest ) ->
            let
                dx =
                    toFloat (target.x * Tiles.size) - rabbit.x

                dy =
                    toFloat (target.y * Tiles.size) - rabbit.y

                distance =
                    sqrt (dx * dx + dy * dy)

                travel =
                    rabbitPixelsPerMillisecond * milliseconds
            in
            if distance <= travel then
                { rabbit | x = rabbit.x + dx, y = rabbit.y + dy, route = rest }

            else
                { rabbit | x = rabbit.x + dx / distance * travel, y = rabbit.y + dy / distance * travel }

        _ ->
            rabbit


rabbitIsGone : Rabbit -> Bool
rabbitIsGone rabbit =
    rabbit.running && List.isEmpty rabbit.route


score : Model -> Contract.Score
score model =
    let
        difficulty =
            Maybe.withDefault Easy model.difficulty
    in
    { tarts = Difficulty.tarts difficulty
    , stats =
        [ { label = .results >> .difficulty, value = \texts -> Difficulty.name texts difficulty }
        , { label = .results >> .keystrokes, value = \_ -> String.fromInt model.keystrokes }
        , { label = .results >> .fewestPossible, value = \_ -> String.fromInt model.fewestKeystrokes }
        , { label = .results >> .usedCount
          , value =
                \texts ->
                    if model.usedCount then
                        texts.results.yes

                    else
                        texts.results.notYet
          }
        ]
    }



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    Story.instructions model.story (scene context model)


{-| Everything the chapter shows. There is no panel (#85): the goal and the
tips are said in dialogs, and the status line keeps what is needed while
walking: the h j k l reminder (or the count being typed) and the position.
-}
scene : Context -> Model -> Contract.View msg
scene context model =
    { stage =
        Html.div [ HA.class "stage-scene" ]
            [ Svg.svg [ SA.viewBox "0 0 320 192", SA.class "pixel-scene" ]
                [ Svg.Lazy.lazy viewMap model.map
                , viewKeySparkle model
                , viewRabbit model.rabbit
                , viewAlice model
                , viewKeyHints model
                , viewPendingCount model
                ]
            , if model.difficulty == Nothing && Story.isPlaying model.story then
                DifficultyChoice.view
                    { texts = context.texts
                    , title = context.texts.hall.chooseTitle
                    , hint = context.texts.hall.chooseHint
                    , info = difficultyInfo context.texts
                    }
                    model.choice

              else
                Html.text ""
            , Story.view context.texts model.story
            ]
    , panel = Html.text ""
    , status =
        { mode = "NOR"
        , file = file
        , pendingKeys =
            if model.pendingCount == "" && model.difficulty /= Nothing && Story.isPlaying model.story then
                "h← j↓ k↑ l→"

            else
                model.pendingCount
        , position = String.fromInt (model.alice.y + 1) ++ ":" ++ String.fromInt (model.alice.x + 1)
        }
    }


difficultyInfo : Texts -> Difficulty -> String
difficultyInfo texts difficulty =
    case difficulty of
        Easy ->
            texts.hall.easyInfo

        Challenge ->
            texts.hall.challengeInfo

        Mad ->
            texts.hall.madInfo


{-| The empty hall, drawn behind the title and results cards.
-}
backdrop : Svg msg
backdrop =
    Svg.svg [ SA.viewBox "0 0 320 192", SA.class "pixel-scene" ]
        [ Svg.Lazy.lazy viewMap startingMap ]


{-| The map as the chapter starts, parsed once. Using the same value every
time lets `Svg.Lazy` skip redrawing it.
-}
startingMap : Map
startingMap =
    Map.parse (Map.plan Easy)


viewMap : Map -> Svg msg
viewMap map =
    map
        |> Array.toIndexedList
        |> List.concatMap
            (\( y, row ) ->
                row
                    |> Array.toIndexedList
                    |> List.map (\( x, tile ) -> viewTile { x = x, y = y } tile)
            )
        |> Svg.g []


viewTile : Point -> Tile -> Svg msg
viewTile position tile =
    if tile == GoldenKey then
        Svg.g []
            [ Tiles.view position tile
            , Sprite.viewAt { x = toFloat (position.x * 16 + 3), y = toFloat (position.y * 16 + 7), pixelSize = 1 } Sprites.goldenKey
            ]

    else
        Tiles.view position tile


viewKeySparkle : Model -> Svg msg
viewKeySparkle model =
    case ( model.hasKey, Map.find GoldenKey model.map ) of
        ( False, Just { x, y } ) ->
            if modBy 2 (floor (model.clock / 330)) == 0 then
                pixelRect (x * 16 + 12) (y * 16 + 4) 1 1 "#ffffff"

            else
                Svg.g [] []

        _ ->
            Svg.g [] []


viewRabbit : Rabbit -> Svg msg
viewRabbit rabbit =
    if rabbitIsGone rabbit then
        Svg.g [] []

    else
        Sprite.viewAt { x = rabbit.x + 2, y = rabbit.y, pixelSize = 1 } Sprites.whiteRabbit


viewAlice : Model -> Svg msg
viewAlice model =
    let
        ( x, y ) =
            aliceOnScreen model

        shake =
            if model.bumpRemaining > 0 then
                sin (model.clock * 0.09)

            else
                0

        sprite =
            if modBy 2 model.stepsTaken == 1 then
                Sprites.aliceStepping

            else
                Sprites.alice
    in
    Sprite.viewAt { x = x + 2 + shake, y = y, pixelSize = 1 } sprite


{-| Alice's pixel position, sliding between tiles while a step is animating.
-}
aliceOnScreen : Model -> ( Float, Float )
aliceOnScreen model =
    let
        toPixels point =
            ( toFloat (point.x * Tiles.size), toFloat (point.y * Tiles.size) )

        ( toX, toY ) =
            toPixels model.alice
    in
    case model.step of
        Just step ->
            let
                ( fromX, fromY ) =
                    toPixels step.from
            in
            ( fromX + (toX - fromX) * step.progress, fromY + (toY - fromY) * step.progress - hop model step )

        Nothing ->
            ( toX, toY )


{-| Jumping over dropped things (#89), Alice is drawn a little higher while
she is above them, rising and landing smoothly.
-}
hop : Model -> Step -> Float
hop model step =
    let
        over point =
            Map.tileAt point model.map == Dropped

        height =
            5
    in
    case ( over step.from, over model.alice ) of
        ( True, True ) ->
            height

        ( False, True ) ->
            height * step.progress

        ( True, False ) ->
            height * (1 - step.progress)

        ( False, False ) ->
            0


{-| Little keycaps next to Alice, showing which key walks where.
-}
viewKeyHints : Model -> Svg msg
viewKeyHints model =
    let
        showHints =
            Story.isPlaying model.story && model.difficulty /= Nothing && model.stepsTaken < stepsWithHints && model.step == Nothing

        keycap ( key, direction ) =
            let
                spot =
                    Map.neighbour direction model.alice
            in
            if Tiles.isWalkable (Map.tileAt spot model.map) then
                [ pixelRect (spot.x * 16 + 3) (spot.y * 16 + 4) 10 10 "#120a18"
                , pixelRect (spot.x * 16 + 3) (spot.y * 16 + 3) 10 9 "#dbbfef"
                , label (spot.x * 16 + 8) (spot.y * 16 + 11) key
                ]

            else
                []
    in
    if showHints then
        Svg.g [] (List.concatMap keycap [ ( "h", Left ), ( "j", Down ), ( "k", Up ), ( "l", Right ) ])

    else
        Svg.g [] []


viewPendingCount : Model -> Svg msg
viewPendingCount model =
    if model.pendingCount == "" then
        Svg.g [] []

    else
        let
            ( x, y ) =
                aliceOnScreen model
        in
        Svg.g []
            [ pixelRect (round x) (round y - 12) 16 10 "#efba5d"
            , label (round x + 8) (round y - 4) model.pendingCount
            ]



-- SVG HELPERS


pixelRect : Int -> Int -> Int -> Int -> String -> Svg msg
pixelRect x y w h color =
    Svg.rect
        [ SA.x (String.fromInt x)
        , SA.y (String.fromInt y)
        , SA.width (String.fromInt w)
        , SA.height (String.fromInt h)
        , SA.fill color
        ]
        []


label : Int -> Int -> String -> Svg msg
label x y text =
    Svg.text_
        [ SA.x (String.fromInt x)
        , SA.y (String.fromInt y)
        , SA.class "pixel-label"
        , SA.textAnchor "middle"
        ]
        [ Svg.text text ]
