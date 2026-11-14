module App exposing
    ( Model, Msg, Command(..), init, update, view
    , needsFrames, keyDecoder, frame, fullscreenChanged
    )

{-| The game shell: settings, screens, and routing between chapters.

This is the one place where chapters meet the outside world. A chapter asks
for things through `Chapter.Contract.Effect`; `applyEffect` below carries
them out (sound, toasts, saving progress). Adding a chapter means adding a
case to `Chapter` and to the four functions under CHAPTERS; the compiler
points at each one.

The shell itself asks the browser for things as data too: `update` returns
`Command`s, and `Main` turns them into ports. That way program tests
(`tests/ProgramFlowTest.elm`) run the whole game, with simulated keys,
frames and ports.

@docs Model, Msg, Command, init, update, view
@docs needsFrames, keyDecoder, frame, fullscreenChanged

-}

import Chapter.Caterpillar as Caterpillar
import Chapter.Contract as Contract exposing (Context, Effect(..), Score)
import Chapter.Curiouser as Curiouser
import Chapter.DrinkMe as DrinkMe
import Chapter.Hall as Hall
import Chapter.Id as ChapterId exposing (ChapterId(..))
import Chapter.LittleDoor as LittleDoor
import Chapter.Pool as Pool
import Chapter.RabbitHole as RabbitHole
import Editor.Session as Session
import Html exposing (Html)
import Html.Attributes as A
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Keyboard.Layout as Layout exposing (Layout)
import Language exposing (Language(..))
import Progress exposing (Progress)
import Project
import Random
import Settings exposing (Settings)
import Texts exposing (Texts)
import Ui.Chrome as Chrome
import Ui.Results
import Ui.TheEnd
import Ui.Title


{-| What the shell asks of the browser. `Main` sends each to its port.
-}
type Command
    = SaveSettings Encode.Value
    | SaveProgress Encode.Value
    | Sound String
    | SetFullscreen Bool



-- MODEL


type alias Model =
    { settings : Settings
    , progress : Progress
    , screen : Screen
    , selectedChapter : ChapterId
    , seed : Random.Seed
    , toast : Maybe Toast
    , links : Project.Links
    , fullscreen : Bool
    , fullscreenHelp : Bool
    }


type Screen
    = Title
    | Playing Chapter
    | Results ChapterId Score
    | TheEnd


{-| One case per chapter.
-}
type Chapter
    = RabbitHoleChapter RabbitHole.Model
    | HallChapter Hall.Model
    | LittleDoorChapter LittleDoor.Model
    | DrinkMeChapter DrinkMe.Model
    | CuriouserChapter Curiouser.Model
    | PoolChapter Pool.Model
    | CaterpillarChapter Caterpillar.Model


type alias Toast =
    { text : Texts -> String, remaining : Float }


toastMilliseconds : Float
toastMilliseconds =
    2800


{-| The most game time one animation frame may move on. When the page was
in the background (another tab, a busy computer), the browser reports the
whole pause at once; without this cap every jar on screen would float away
in one go. 100 ms keeps the game in real time down to 10 frames a second.
-}
longestFrameMilliseconds : Float
longestFrameMilliseconds =
    100


{-| Flags from JavaScript: the browser language, a random seed, where the
game is served from (for its links), and anything saved earlier. Bad or missing data never stops the game; it falls back to
defaults.
-}
init : Decode.Value -> ( Model, List Command )
init flags =
    let
        decodeFlag field decoder fallback =
            Decode.decodeValue (Decode.field field decoder) flags
                |> Result.withDefault fallback

        defaults =
            Settings.guess (decodeFlag "browserLanguage" Decode.string "en")

        progress =
            decodeFlag "progress" Progress.decoder Progress.empty
    in
    ( { settings = decodeFlag "settings" (Settings.decoder defaults) defaults
      , progress = progress
      , screen = Title
      , selectedChapter = firstUnfinished progress
      , seed = Random.initialSeed (decodeFlag "seed" Decode.int 1865)
      , toast = Nothing
      , fullscreen = False
      , fullscreenHelp = False
      , links = decodeFlag "links" Project.decoder Project.fallback
      }
    , []
    )


{-| Where a returning player probably wants to continue.
-}
firstUnfinished : Progress -> ChapterId
firstUnfinished progress =
    ChapterId.all
        |> List.filter (\id -> Progress.tartsFor id progress == 0)
        |> List.head
        |> Maybe.withDefault RabbitHole



-- UPDATE


type Msg
    = KeyDown KeyPress
    | Frame Float
    | ChoseLanguage Language
    | ChoseLayout Layout
    | ToggledSound
    | ToggledUnlockAll
    | OpenedMenu
    | ChoseChapter ChapterId
    | StartedChapter ChapterId
    | FinishedGame
    | ToggledFullscreen
    | FullscreenChanged Bool


type alias KeyPress =
    { key : String, code : String }


update : Msg -> Model -> ( Model, List Command )
update msg model =
    case msg of
        KeyDown press ->
            if model.fullscreenHelp then
                -- The card about j k: Enter closes it.
                if press.key == "Enter" then
                    ( { model | fullscreenHelp = False }, [] )

                else
                    ( model, [] )

            else
                detectLayout press model
                    |> andThen (handleKey press.key)

        Frame elapsed ->
            let
                milliseconds =
                    min longestFrameMilliseconds elapsed
            in
            { model | toast = countDown milliseconds model.toast }
                |> stepCurrentChapter (Tick milliseconds)

        ChoseLanguage language ->
            saveSettings (\s -> { s | language = language }) model

        ChoseLayout layout ->
            saveSettings (\s -> { s | layout = layout }) model

        ToggledSound ->
            saveSettings (\s -> { s | soundOn = not s.soundOn }) model

        ToggledFullscreen ->
            -- Entering fullscreen explains j k first, every time: the browser
            -- will use Esc to leave fullscreen (#110).
            ( { model
                | fullscreen = not model.fullscreen
                , fullscreenHelp = not model.fullscreen
              }
            , [ SetFullscreen (not model.fullscreen) ]
            )

        FullscreenChanged inBrowserFullscreen ->
            -- Leaving the browser's fullscreen (holding Esc, F11) leaves the
            -- fullscreen layout too. Entering it changes nothing: the layout
            -- was switched on already.
            if inBrowserFullscreen then
                ( model, [] )

            else
                ( { model | fullscreen = False, fullscreenHelp = False }, [] )

        ToggledUnlockAll ->
            saveSettings (\s -> { s | unlockAll = not s.unlockAll }) model

        OpenedMenu ->
            ( { model | screen = Title }, [] )

        ChoseChapter id ->
            chooseChapter id model

        StartedChapter id ->
            startChapter id model

        FinishedGame ->
            ( { model | screen = TheEnd }, [] )


handleKey : String -> Model -> ( Model, List Command )
handleKey key model =
    case model.screen of
        Title ->
            case key of
                "j" ->
                    ( { model | selectedChapter = shiftChapter 1 model.selectedChapter }, [] )

                "k" ->
                    ( { model | selectedChapter = shiftChapter -1 model.selectedChapter }, [] )

                "ArrowDown" ->
                    ( { model | selectedChapter = shiftChapter 1 model.selectedChapter }, [] )

                "ArrowUp" ->
                    ( { model | selectedChapter = shiftChapter -1 model.selectedChapter }, [] )

                "Enter" ->
                    chooseChapter model.selectedChapter model

                _ ->
                    ( model, [] )

        Playing _ ->
            stepCurrentChapter (Key key) model

        Results id _ ->
            case key of
                "Enter" ->
                    afterResults id model

                "r" ->
                    startChapter id model

                "m" ->
                    ( { model | screen = Title }, [] )

                _ ->
                    ( model, [] )

        TheEnd ->
            if key == "Enter" || key == "m" then
                ( { model | screen = Title }, [] )

            else
                ( model, [] )


{-| From the title screen: start the chapter if it is open, otherwise say
why not.
-}
chooseChapter : ChapterId -> Model -> ( Model, List Command )
chooseChapter id model =
    if Progress.isOpen { unlockAll = model.settings.unlockAll } id model.progress then
        startChapter id model

    else
        ( { model | selectedChapter = id, toast = Just { text = .finishEarlierChapter, remaining = toastMilliseconds } }
        , playSound model "error"
        )


startChapter : ChapterId -> Model -> ( Model, List Command )
startChapter id model =
    let
        ( chapterSeed, nextSeed ) =
            Random.step Random.independentSeed model.seed
    in
    ( { model
        | screen = Playing (newChapter chapterSeed id)
        , selectedChapter = id
        , seed = nextSeed
        , toast = Nothing
      }
    , []
    )


{-| Enter on the results card: on to the next chapter if it is open, the
end card after the last one, otherwise play this chapter again.
-}
afterResults : ChapterId -> Model -> ( Model, List Command )
afterResults id model =
    case nextAfterResults id model of
        Just msg ->
            update msg model

        Nothing ->
            startChapter id model


{-| What "Next" does on the results card, or `Nothing` while the next
chapter is still locked.
-}
nextAfterResults : ChapterId -> Model -> Maybe Msg
nextAfterResults id model =
    case ChapterId.next id of
        Just next ->
            if Progress.isOpen { unlockAll = model.settings.unlockAll } next model.progress then
                Just (StartedChapter next)

            else
                Nothing

        Nothing ->
            Just FinishedGame


stepCurrentChapter : ChapterInput -> Model -> ( Model, List Command )
stepCurrentChapter input model =
    case model.screen of
        Playing chapter ->
            let
                ( next, effects ) =
                    stepChapter (context model) input chapter
            in
            applyEffects (chapterId chapter) effects { model | screen = Playing next }

        _ ->
            ( model, [] )


applyEffects : ChapterId -> List Effect -> Model -> ( Model, List Command )
applyEffects id effects model =
    List.foldl (\effect ( current, commands ) -> applyEffect id effect current |> Tuple.mapSecond ((++) commands))
        ( model, [] )
        effects


applyEffect : ChapterId -> Effect -> Model -> ( Model, List Command )
applyEffect id effect model =
    case effect of
        PlaySound sound ->
            ( model, playSound model (Contract.soundName sound) )

        ShowToast text ->
            ( { model | toast = Just { text = text, remaining = toastMilliseconds } }, [] )

        Finish score ->
            let
                progress =
                    Progress.record id score.tarts model.progress
            in
            ( { model | screen = Results id score, progress = progress, toast = Nothing }
            , SaveProgress (Progress.encode progress) :: playSound model "fanfare"
            )


{-| If a key press shows the keyboard layout setting is wrong, fix it and
say so.
-}
detectLayout : KeyPress -> Model -> ( Model, List Command )
detectLayout press model =
    case Layout.detect model.settings.layout press of
        Just layout ->
            saveSettings (\s -> { s | layout = layout })
                { model | toast = Just { text = \t -> t.layoutDetected (Layout.name layout), remaining = toastMilliseconds } }

        Nothing ->
            ( model, [] )


saveSettings : (Settings -> Settings) -> Model -> ( Model, List Command )
saveSettings change model =
    let
        settings =
            change model.settings
    in
    ( { model | settings = settings }, [ SaveSettings (Settings.encode settings) ] )


playSound : Model -> String -> List Command
playSound model name =
    if model.settings.soundOn then
        [ Sound name ]

    else
        []


context : Model -> Context
context model =
    { texts = Texts.forLanguage model.settings.language
    , layout = model.settings.layout
    }


shiftChapter : Int -> ChapterId -> ChapterId
shiftChapter delta id =
    let
        count =
            List.length ChapterId.all

        target =
            modBy count (ChapterId.number id - 1 + delta) + 1
    in
    ChapterId.all
        |> List.filter (\other -> ChapterId.number other == target)
        |> List.head
        |> Maybe.withDefault id


countDown : Float -> Maybe { a | remaining : Float } -> Maybe { a | remaining : Float }
countDown milliseconds timer =
    timer
        |> Maybe.map (\t -> { t | remaining = t.remaining - milliseconds })
        |> Maybe.andThen
            (\t ->
                if t.remaining > 0 then
                    Just t

                else
                    Nothing
            )


andThen : (Model -> ( Model, List Command )) -> ( Model, List Command ) -> ( Model, List Command )
andThen next ( model, commands ) =
    next model |> Tuple.mapSecond ((++) commands)



-- CHAPTERS


type ChapterInput
    = Key String
    | Tick Float


newChapter : Random.Seed -> ChapterId -> Chapter
newChapter seed id =
    case id of
        RabbitHole ->
            RabbitHoleChapter (RabbitHole.init seed)

        HallOfDoors ->
            HallChapter Hall.init

        LittleDoor ->
            LittleDoorChapter LittleDoor.init

        DrinkMe ->
            DrinkMeChapter DrinkMe.init

        Curiouser ->
            CuriouserChapter (Curiouser.init seed)

        PoolOfTears ->
            PoolChapter (Pool.init seed)

        CaterpillarsAdvice ->
            CaterpillarChapter Caterpillar.init


chapterId : Chapter -> ChapterId
chapterId chapter =
    case chapter of
        RabbitHoleChapter _ ->
            RabbitHole

        HallChapter _ ->
            HallOfDoors

        LittleDoorChapter _ ->
            LittleDoor

        DrinkMeChapter _ ->
            DrinkMe

        CuriouserChapter _ ->
            Curiouser

        PoolChapter _ ->
            PoolOfTears

        CaterpillarChapter _ ->
            CaterpillarsAdvice


{-| Every chapter has the same two messages, a key and a frame; this turns
the shell's input into the chapter's own `Msg`.
-}
stepChapter : Context -> ChapterInput -> Chapter -> ( Chapter, List Effect )
stepChapter ctx input chapter =
    let
        run chapterUpdate keyMsg frameMsg wrap model =
            let
                msg =
                    case input of
                        Key key ->
                            keyMsg key

                        Tick milliseconds ->
                            frameMsg milliseconds
            in
            chapterUpdate ctx msg model |> Tuple.mapFirst wrap
    in
    case chapter of
        RabbitHoleChapter model ->
            run RabbitHole.update RabbitHole.KeyPressed RabbitHole.Frame RabbitHoleChapter model

        HallChapter model ->
            run Hall.update Hall.KeyPressed Hall.Frame HallChapter model

        LittleDoorChapter model ->
            run LittleDoor.update LittleDoor.KeyPressed LittleDoor.Frame LittleDoorChapter model

        DrinkMeChapter model ->
            run DrinkMe.update DrinkMe.KeyPressed DrinkMe.Frame DrinkMeChapter model

        CuriouserChapter model ->
            run Curiouser.update Curiouser.KeyPressed Curiouser.Frame CuriouserChapter model

        PoolChapter model ->
            run Pool.update Pool.KeyPressed Pool.Frame PoolChapter model

        CaterpillarChapter model ->
            run Caterpillar.update Caterpillar.KeyPressed Caterpillar.Frame CaterpillarChapter model


viewChapter : Context -> Chapter -> Contract.View Msg
viewChapter ctx chapter =
    case chapter of
        RabbitHoleChapter model ->
            RabbitHole.view ctx model

        HallChapter model ->
            Hall.view ctx model

        LittleDoorChapter model ->
            LittleDoor.view ctx model

        DrinkMeChapter model ->
            DrinkMe.view ctx model

        CuriouserChapter model ->
            Curiouser.view ctx model

        PoolChapter model ->
            Pool.view ctx model

        CaterpillarChapter model ->
            Caterpillar.view ctx model



-- SUBSCRIPTIONS


{-| Animation frames are needed only while something can move: during a
chapter, or while a toast is fading. The title and results screens then
cost nothing while the player is reading.
-}
needsFrames : Model -> Bool
needsFrames model =
    case model.screen of
        Playing _ ->
            True

        _ ->
            model.toast /= Nothing


{-| A frame of `milliseconds`, from the browser's animation frames.
-}
frame : Float -> Msg
frame =
    Frame


{-| The browser entered (True) or left (False) its fullscreen.
-}
fullscreenChanged : Bool -> Msg
fullscreenChanged =
    FullscreenChanged


{-| Plain key presses only: browser shortcuts (Ctrl, Alt, Cmd) are left
alone, except Ctrl+[ which terminals and Helix treat as Escape.
-}
keyDecoder : Decoder Msg
keyDecoder =
    Decode.map5
        (\key code ctrl alt meta ->
            if ctrl && (key == "[" || code == "BracketLeft") then
                Decode.succeed (KeyDown { key = "Escape", code = "Escape" })

            else if ctrl || alt || meta then
                Decode.fail "shortcut"

            else
                Decode.succeed (KeyDown { key = key, code = code })
        )
        (Decode.field "key" Decode.string)
        (Decode.field "code" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        |> Decode.andThen identity



-- VIEW


view : Model -> Html Msg
view model =
    let
        ctx =
            context model

        texts =
            ctx.texts

        screen =
            case model.screen of
                Playing chapter ->
                    viewChapter ctx chapter

                Title ->
                    cardScreen
                        (Ui.Title.view
                            { texts = texts
                            , progress = model.progress
                            , selected = model.selectedChapter
                            , unlockAll = model.settings.unlockAll
                            , onStart = ChoseChapter
                            , onToggleUnlock = ToggledUnlockAll
                            }
                        )
                        (Html.p [ A.class "big" ] [ Html.text texts.tagline ])

                Results id score ->
                    cardScreen
                        (Ui.Results.view
                            { texts = texts
                            , chapter = id
                            , score = score
                            , onNext = nextAfterResults id model
                            , onReplay = StartedChapter id
                            , onMenu = OpenedMenu
                            }
                        )
                        (Html.p [ A.class "lesson" ] [ Html.text (texts.chapter id).lesson ])

                TheEnd ->
                    cardScreen
                        (Ui.TheEnd.view { texts = texts, totalTarts = Progress.total model.progress, onMenu = OpenedMenu })
                        (Html.p [ A.class "big" ] [ Html.text texts.theEnd.teaser ])
    in
    Html.div [ A.classList [ ( "app", True ), ( "fullscreen", model.fullscreen ) ] ]
        [ Html.Lazy.lazy2 viewHeader texts model.settings
        , Html.main_ [ A.class "stage" ]
            [ screen.stage
            , case model.toast of
                Just toast ->
                    Html.div [ A.class "toast", A.attribute "role" "status" ] [ Html.text (toast.text texts) ]

                Nothing ->
                    Html.text ""
            , if model.fullscreenHelp then
                viewFullscreenHelp texts

              else
                Html.text ""
            ]
        , Html.section [ A.class "panel", A.attribute "aria-live" "polite" ] [ screen.panel ]
        , Chrome.statusLine
            (if model.fullscreen then
                Just { onExit = ToggledFullscreen, label = texts.exitFullscreen }

             else
                Nothing
            )
            screen.status
        , Html.Lazy.lazy3 viewFooter model.links texts (screenName model)
        ]


{-| The card shown each time fullscreen starts (#110): Esc leaves the
browser's fullscreen, so back to NORMAL is `j k`, as with this Helix
config. Enter closes it.
-}
viewFullscreenHelp : Texts -> Html Msg
viewFullscreenHelp texts =
    Html.div [ A.class "overlay" ]
        [ Html.div [ A.class "card" ]
            [ Html.h2 [] [ Html.text texts.fullscreenHelp.title ]
            , Html.p [] [ Html.text texts.fullscreenHelp.explain ]
            , Html.p [ A.class "hint" ] [ Html.text texts.fullscreenHelp.config ]
            , Html.pre [ A.class "config" ] [ Html.text Session.escapeConfig ]
            , Html.p [ A.class "hint" ] [ Html.text texts.continueHint ]
            ]
        ]


{-| Header and footer change only with the settings or the screen. They are
drawn through `Html.Lazy`, which skips them while their arguments stay the
same, instead of rebuilding them 60 times a second.
-}
viewHeader : Texts -> Settings -> Html Msg
viewHeader texts settings =
    Chrome.header
        { texts = texts
        , settings = settings
        , onLanguage = ChoseLanguage
        , onLayout = ChoseLayout
        , onToggleSound = ToggledSound
        , onMenu = OpenedMenu
        , onFullscreen = ToggledFullscreen
        }


viewFooter : Project.Links -> Texts -> String -> Html Msg
viewFooter links texts screen =
    Html.footer [ A.class "page-footer" ]
        [ Html.nav [ A.class "footer-links" ]
            [ Html.a [ A.href (Project.pixelEditorUrl links), A.target "_blank", A.rel "noopener" ] [ Html.text texts.drawPictures ]
            , Html.a [ A.href (Project.proposeEditUrl links screen), A.target "_blank", A.rel "noopener" ] [ Html.text texts.suggestChange ]
            ]
        , Html.p [ A.class "footer" ] [ Html.text texts.footer ]
        ]


{-| Which screen a change proposal is about, written in English for whoever
reads the issue, with the language the player had chosen.
-}
screenName : Model -> String
screenName model =
    let
        chapterLabel id =
            "Chapter " ++ String.fromInt (ChapterId.number id) ++ ": " ++ ((Texts.forLanguage English).chapter id).name

        where_ =
            case model.screen of
                Title ->
                    "Title screen"

                Playing chapter ->
                    chapterLabel (chapterId chapter)

                Results id _ ->
                    "Results of " ++ chapterLabel id

                TheEnd ->
                    "End screen"
    in
    where_ ++ " (" ++ Language.toCode model.settings.language ++ ")"


{-| Title, results and the end: a card over the empty hall.
-}
cardScreen : Html Msg -> Html Msg -> Contract.View Msg
cardScreen card panel =
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Hall.backdrop
            , Html.div [ A.class "overlay" ] [ card ]
            ]
    , panel = panel
    , status = { mode = "NORMAL", file = "wonderland.txt", pendingKeys = "", position = "1:1" }
    }
