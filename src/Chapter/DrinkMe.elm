module Chapter.DrinkMe exposing (Model, Msg(..), init, update, view, wordsPerMinute)

{-| Chapter 3, "Drink Me": typing whole words. Each word is one sip from the
bottle and Alice shrinks a little, until she is small enough for the little
door. Adds two new keys, `e` and `i`, and the thumb on the space bar.

Typing speed is measured with the game's own clock (the `Frame` messages),
not the computer's, so tests can type at any pace they like.

-}

import Chapter.Contract as Contract exposing (Context, Effect(..), Sound(..))
import Chapter.Story as Story exposing (Story)
import Html exposing (Html)
import Html.Attributes as A
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Speaker exposing (Speaker(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Texts exposing (Texts)
import Ui.Dialog as Dialog


file : String
file =
    "03_drink_me.txt"



-- TUNING


{-| Alice's size in screen pixels per sprite pixel, before the first sip and
after the last.
-}
tallest : Float
tallest =
    6


smallest : Float
smallest =
    1.4


walkMilliseconds : Float
walkMilliseconds =
    1400


{-| Speed is only shown once there is enough typing to measure it.
-}
millisecondsBeforeSpeed : Float
millisecondsBeforeSpeed =
    3000



-- MODEL


type alias Model =
    { story : Story
    , words : Maybe (List String)
    , wordIndex : Int
    , letterIndex : Int
    , waitingForSpace : Bool
    , correct : Int
    , errors : Int
    , lastKeyWrong : Bool
    , typingTime : Float
    , alicePixelSize : Float
    , walk : Maybe Float
    , clock : Float
    }


init : Model
init =
    { story =
        Story.begin (Dialog.say Narrator (.drinkMe >> .intro1))
            [ Dialog.say Narrator (.drinkMe >> .intro2)
            , Dialog.say Narrator (.drinkMe >> .intro3)
            ]
    , words = Nothing
    , wordIndex = 0
    , letterIndex = 0
    , waitingForSpace = False
    , correct = 0
    , errors = 0
    , lastKeyWrong = False
    , typingTime = 0
    , alicePixelSize = tallest
    , walk = Nothing
    , clock = 0
    }


{-| The word list follows the language setting until the first key press,
then stays fixed for the rest of the chapter.
-}
currentWords : Texts -> Model -> List String
currentWords texts model =
    Maybe.withDefault texts.drinkMe.words model.words


{-| The key to press next: a letter of the current word, or the space bar
after the word.
-}
expectedKey : List String -> Model -> Maybe Char
expectedKey words model =
    if model.waitingForSpace then
        Just ' '

    else
        words
            |> List.drop model.wordIndex
            |> List.head
            |> Maybe.andThen (String.toList >> List.drop model.letterIndex >> List.head)



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
                        typeKey context.texts key model

                    else
                        ( model, [] )

                Story.Swallowed ->
                    ( model, [] )

                Story.Advanced story ->
                    ( { model | story = story }, [ PlaySound TickSound ] )

                Story.IntroOver story ->
                    ( { model | story = story }, [ ShowToast (.drinkMe >> .newKeys) ] )

                Story.OutroOver story ->
                    ( { model | story = story }, [ Finish (score model) ] )

        Frame milliseconds ->
            frame context.texts milliseconds model


typeKey : Texts -> String -> Model -> ( Model, List Effect )
typeKey texts key model =
    case ( model.walk, String.toList (String.toLower key) ) of
        ( Nothing, [ char ] ) ->
            let
                words =
                    currentWords texts model

                started =
                    { model | words = Just words }
            in
            if Just char == expectedKey words started then
                typedRight words char started

            else
                ( { started | errors = model.errors + 1, lastKeyWrong = True }
                , [ PlaySound ErrorSound, FlashKey (String.fromChar char) False ]
                )

        _ ->
            ( model, [] )


{-| A right key: a letter moves along the word, space moves to the next
word. The last word needs no space: its last letter is the last sip.
-}
typedRight : List String -> Char -> Model -> ( Model, List Effect )
typedRight words char model =
    if model.waitingForSpace then
        ( { model
            | waitingForSpace = False
            , wordIndex = model.wordIndex + 1
            , letterIndex = 0
            , correct = model.correct + 1
            , lastKeyWrong = False
          }
        , [ PlaySound CorrectSound, FlashKey " " True ]
        )

    else
        let
            letterIndex =
                model.letterIndex + 1

            wordDone =
                letterIndex >= (words |> List.drop model.wordIndex |> List.head |> Maybe.map String.length |> Maybe.withDefault 0)

            lastWord =
                model.wordIndex == List.length words - 1

            typed =
                { model | letterIndex = letterIndex, correct = model.correct + 1, lastKeyWrong = False }
        in
        if wordDone && lastWord then
            -- The last sip: Alice walks to the door, then the closing story.
            ( { typed | wordIndex = model.wordIndex + 1, letterIndex = 0, walk = Just 0 }
            , [ PlaySound PickupSound, FlashKey (String.fromChar char) True ]
            )

        else
            ( { typed | waitingForSpace = wordDone }
            , [ PlaySound TickSound, FlashKey (String.fromChar char) True ]
            )


frame : Texts -> Float -> Model -> ( Model, List Effect )
frame texts milliseconds model =
    let
        words =
            currentWords texts model

        targetSize =
            tallest - (tallest - smallest) * toFloat model.wordIndex / toFloat (max 1 (List.length words))

        typing =
            model.words /= Nothing && model.walk == Nothing

        advanced =
            { model
                | clock = model.clock + milliseconds
                , story = Story.tick milliseconds model.story
                , typingTime =
                    if typing then
                        model.typingTime + milliseconds

                    else
                        model.typingTime
                , alicePixelSize = model.alicePixelSize + (targetSize - model.alicePixelSize) * min 1 (milliseconds * 0.004)
                , walk = Maybe.map (\progress -> min 1 (progress + milliseconds / walkMilliseconds)) model.walk
            }
    in
    case advanced.walk of
        Just progress ->
            if progress >= 1 && Story.isPlaying advanced.story then
                ( { advanced | story = Story.conclude (Dialog.say Narrator (.drinkMe >> .finale)) [] }, [] )

            else
                ( advanced, [] )

        Nothing ->
            ( advanced, [] )


accuracy : Model -> Float
accuracy model =
    if model.correct + model.errors == 0 then
        1

    else
        toFloat model.correct / toFloat (model.correct + model.errors)


{-| Words per minute, counting five keystrokes as one word, as typing tutors do.
-}
wordsPerMinute : Model -> Int
wordsPerMinute model =
    if model.typingTime < millisecondsBeforeSpeed then
        0

    else
        round ((toFloat model.correct / 5) / (model.typingTime / 60000))


percent : Float -> String
percent fraction =
    String.fromInt (round (fraction * 100)) ++ "%"


score : Model -> Contract.Score
score model =
    let
        fraction =
            accuracy model
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
        , { label = .results >> .wordsPerMinute, value = \_ -> String.fromInt (wordsPerMinute model) }
        ]
    }



-- VIEW


view : Context -> Model -> Contract.View msg
view context model =
    let
        words =
            currentWords context.texts model

        target =
            if Story.isPlaying model.story && model.walk == Nothing then
                expectedKey words model

            else
                Nothing
    in
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Svg.svg [ SA.viewBox "0 0 320 192", SA.class "pixel-scene" ] (viewHall context.texts model)
            , Story.view context.texts model.story
            ]
    , panel = viewPanel context.texts words model
    , status =
        { mode = "INS"
        , file = file
        , pendingKeys = ""
        , position = String.fromInt model.wordIndex ++ "/" ++ String.fromInt (List.length words)
        }
    , keyboard = { target = target, helpers = [] }
    }


{-| The hall seen from the side: the glass table with the bottle, the little
door behind its curtain, and Alice getting smaller.
-}
viewHall : Texts -> Model -> List (Svg msg)
viewHall texts model =
    let
        label =
            texts.drinkMe.bottle

        -- The label font is 7 px per character at 11 px.
        labelWidth =
            toFloat (String.length label * 7 + 8)

        pixelSize =
            model.alicePixelSize

        walk =
            Maybe.withDefault 0 model.walk

        aliceCentre =
            150 + (283 - 150) * walk

        walking =
            model.walk /= Nothing && walk < 1 && modBy 2 (floor (model.clock / 125)) == 1
    in
    rect 0 0 320 150 "#4a3060"
        :: List.map (\x -> rect (toFloat x) 0 1 150 "#432a58") (List.range 0 13 |> List.map ((*) 24))
        ++ [ rect 0 138 320 12 "#5a3d73" ]
        ++ List.concatMap
            (\x ->
                List.map
                    (\y ->
                        rect (toFloat (x * 16))
                            (toFloat (150 + y * 16))
                            16
                            16
                            (if modBy 2 (x + y) == 0 then
                                "#3c2a4b"

                             else
                                "#33223f"
                            )
                    )
                    [ 0, 1, 2 ]
            )
            (List.range 0 19)
        ++ [ -- the curtain and the little door
             rect 272 112 22 38 "#b8435a"
           , rect 278 126 12 24 "#8a5a3c"
           , rect 287 138 2 2 "#efba5d"

           -- the glass table
           , translucent 30 108 70 4 "#bfe6f0" 0.35
           , rect 30 108 70 1 "#bfe6f0"
           , translucent 34 112 2 38 "#bfe6f0" 0.45
           , translucent 94 112 2 38 "#bfe6f0" 0.45

           -- the bottle with its paper label on a string
           , rect 62 86 6 3 "#8a5a3c"
           , translucent 63 89 4 4 "#a4a0e8" 0.7
           , translucent 59 93 12 15 "#a4a0e8" 0.7
           , rect 60 97 10 6 "#f4ead2"
           , rect 65 74 1 12 "#dbbfef"
           , rect (65 - labelWidth / 2) 62 labelWidth 12 "#f4ead2"
           , rect (65 - labelWidth / 2) 73 labelWidth 1 "#120a18"
           , Svg.text_ [ SA.x "65", SA.y "71", SA.class "bottle-label", SA.textAnchor "middle" ] [ Svg.text label ]
           , Sprite.viewAt
                { x = aliceCentre - 6 * pixelSize
                , y = 150 - 16 * pixelSize
                , pixelSize = pixelSize
                }
                (if walking then
                    Sprites.aliceStepping

                 else
                    Sprites.alice
                )
           ]


viewPanel : Texts -> List String -> Model -> Html msg
viewPanel texts words model =
    let
        stat name value =
            Html.span [] [ Html.text name, Html.b [] [ Html.text value ] ]

        firstShown =
            max 0 (model.wordIndex - 2)
    in
    Html.div [ A.class "panel-content" ]
        [ Html.div [ A.class "words" ]
            (words
                |> List.indexedMap Tuple.pair
                |> List.drop firstShown
                |> List.take 6
                |> List.map (\( index, word ) -> viewWord model index word)
            )
        , Html.div [ A.class "stats" ]
            [ stat texts.drinkMe.sips (String.fromInt model.wordIndex ++ "/" ++ String.fromInt (List.length words))
            , stat texts.results.accuracy (percent (accuracy model))
            , stat texts.results.wordsPerMinute (String.fromInt (wordsPerMinute model))
            ]
        ]


viewWord : Model -> Int -> String -> Html msg
viewWord model index word =
    if index < model.wordIndex then
        Html.span [ A.class "word done" ] [ Html.text word ]

    else if index > model.wordIndex then
        Html.span [ A.class "word next" ] [ Html.text word ]

    else
        let
            letter position char =
                Html.span
                    [ A.classList
                        [ ( "typed", position < model.letterIndex )
                        , ( "current", position == model.letterIndex && not model.waitingForSpace )
                        , ( "wrong", position == model.letterIndex && not model.waitingForSpace && model.lastKeyWrong )
                        ]
                    ]
                    [ Html.text (String.fromChar char) ]

            -- The space bar's place is always there, drawn inside the gap after
            -- the word, so the line never changes width. Only its underline
            -- appears when it is time to press space.
            spaceSlot =
                Html.span
                    [ A.classList
                        [ ( "space-slot", True )
                        , ( "current", model.waitingForSpace )
                        , ( "wrong", model.waitingForSpace && model.lastKeyWrong )
                        ]
                    ]
                    [ Html.text "\u{00A0}" ]
        in
        Html.span [ A.class "word now" ] (List.indexedMap letter (String.toList word) ++ [ spaceSlot ])



-- SVG HELPERS


rect : Float -> Float -> Float -> Float -> String -> Svg msg
rect x y w h color =
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , SA.fill color
        ]
        []


translucent : Float -> Float -> Float -> Float -> String -> Float -> Svg msg
translucent x y w h color opacity =
    Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y (String.fromFloat y)
        , SA.width (String.fromFloat w)
        , SA.height (String.fromFloat h)
        , SA.fill color
        , SA.fillOpacity (String.fromFloat opacity)
        ]
        []
