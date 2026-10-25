module Main exposing (main)

{-| The game shell: settings, screens, and routing between chapters.

This is the one place where chapters meet the outside world. A chapter asks
for things through `Chapter.Contract.Effect`; `applyEffect` below carries
them out (sound, toasts, saving progress). Adding a chapter means adding a
case to `Chapter` and to the four functions under CHAPTERS; the compiler
points at each one.

-}

import Browser
import Browser.Events
import Chapter.Caterpillar as Caterpillar
import Chapter.Contract as Contract exposing (Context, Effect(..), Score)
import Chapter.DrinkMe as DrinkMe
import Chapter.Hall as Hall
import Chapter.Id as ChapterId exposing (ChapterId(..))
import Chapter.Pool as Pool
import Chapter.RabbitHole as RabbitHole
import Html exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode exposing (Decoder)
import Keyboard.Layout as Layout exposing (Layout)
import Keyboard.View
import Language exposing (Language(..))
import Ports
import Progress exposing (Progress)
import Project
import Random
import Settings exposing (Settings)
import Texts exposing (Texts)
import Ui.Chrome as Chrome
import Ui.Results
import Ui.TheEnd
import Ui.Title


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { settings : Settings
    , progress : Progress
    , screen : Screen
    , selectedChapter : ChapterId
    , seed : Random.Seed
    , toast : Maybe Toast
    , flash : Maybe Flash
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
    | DrinkMeChapter DrinkMe.Model
    | PoolChapter Pool.Model
    | CaterpillarChapter Caterpillar.Model


type alias Toast =
    { text : Texts -> String, remaining : Float }


type alias Flash =
    { key : String, ok : Bool, remaining : Float }


toastMilliseconds : Float
toastMilliseconds =
    2800


flashMilliseconds : Float
flashMilliseconds =
    170


{-| Flags from JavaScript: the browser language, a random seed, and anything
saved earlier. Bad or missing data never stops the game; it falls back to
defaults.
-}
init : Decode.Value -> ( Model, Cmd Msg )
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
      , flash = Nothing
      }
    , Cmd.none
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


type alias KeyPress =
    { key : String, code : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown press ->
            detectLayout press model
                |> andThen (handleKey press.key)

        Frame milliseconds ->
            { model
                | toast = countDown milliseconds model.toast
                , flash = countDown milliseconds model.flash
            }
                |> stepCurrentChapter (Tick milliseconds)

        ChoseLanguage language ->
            saveSettings (\s -> { s | language = language }) model

        ChoseLayout layout ->
            saveSettings (\s -> { s | layout = layout }) model

        ToggledSound ->
            saveSettings (\s -> { s | soundOn = not s.soundOn }) model

        ToggledUnlockAll ->
            saveSettings (\s -> { s | unlockAll = not s.unlockAll }) model

        OpenedMenu ->
            ( { model | screen = Title }, Cmd.none )

        ChoseChapter id ->
            chooseChapter id model

        StartedChapter id ->
            startChapter id model

        FinishedGame ->
            ( { model | screen = TheEnd }, Cmd.none )


handleKey : String -> Model -> ( Model, Cmd Msg )
handleKey key model =
    case model.screen of
        Title ->
            case key of
                "j" ->
                    ( { model | selectedChapter = shiftChapter 1 model.selectedChapter }, Cmd.none )

                "k" ->
                    ( { model | selectedChapter = shiftChapter -1 model.selectedChapter }, Cmd.none )

                "ArrowDown" ->
                    ( { model | selectedChapter = shiftChapter 1 model.selectedChapter }, Cmd.none )

                "ArrowUp" ->
                    ( { model | selectedChapter = shiftChapter -1 model.selectedChapter }, Cmd.none )

                "Enter" ->
                    chooseChapter model.selectedChapter model

                _ ->
                    ( model, Cmd.none )

        Playing _ ->
            stepCurrentChapter (Key key) model

        Results id _ ->
            case key of
                "Enter" ->
                    afterResults id model

                "r" ->
                    startChapter id model

                "m" ->
                    ( { model | screen = Title }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TheEnd ->
            if key == "Enter" || key == "m" then
                ( { model | screen = Title }, Cmd.none )

            else
                ( model, Cmd.none )


{-| From the title screen: start the chapter if it is open, otherwise say
why not.
-}
chooseChapter : ChapterId -> Model -> ( Model, Cmd Msg )
chooseChapter id model =
    if Progress.isOpen { unlockAll = model.settings.unlockAll } id model.progress then
        startChapter id model

    else
        ( { model | selectedChapter = id, toast = Just { text = .finishEarlierChapter, remaining = toastMilliseconds } }
        , playSound model "error"
        )


startChapter : ChapterId -> Model -> ( Model, Cmd Msg )
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
    , Cmd.none
    )


afterResults : ChapterId -> Model -> ( Model, Cmd Msg )
afterResults id model =
    case ChapterId.next id of
        Just next ->
            startChapter next model

        Nothing ->
            ( { model | screen = TheEnd }, Cmd.none )


stepCurrentChapter : ChapterInput -> Model -> ( Model, Cmd Msg )
stepCurrentChapter input model =
    case model.screen of
        Playing chapter ->
            let
                ( next, effects ) =
                    stepChapter (context model) input chapter
            in
            applyEffects (chapterId chapter) effects { model | screen = Playing next }

        _ ->
            ( model, Cmd.none )


applyEffects : ChapterId -> List Effect -> Model -> ( Model, Cmd Msg )
applyEffects id effects model =
    List.foldl (\effect ( current, cmd ) -> applyEffect id effect current |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ]))
        ( model, Cmd.none )
        effects


applyEffect : ChapterId -> Effect -> Model -> ( Model, Cmd Msg )
applyEffect id effect model =
    case effect of
        PlaySound sound ->
            ( model, playSound model (Contract.soundName sound) )

        ShowToast text ->
            ( { model | toast = Just { text = text, remaining = toastMilliseconds } }, Cmd.none )

        FlashKey key ok ->
            ( { model | flash = Just { key = key, ok = ok, remaining = flashMilliseconds } }, Cmd.none )

        Finish score ->
            let
                progress =
                    Progress.record id score.tarts model.progress
            in
            ( { model | screen = Results id score, progress = progress, toast = Nothing }
            , Cmd.batch [ Ports.saveProgress (Progress.encode progress), playSound model "fanfare" ]
            )


{-| If a key press shows the keyboard layout setting is wrong, fix it and
say so.
-}
detectLayout : KeyPress -> Model -> ( Model, Cmd Msg )
detectLayout press model =
    case Layout.detect model.settings.layout press of
        Just layout ->
            saveSettings (\s -> { s | layout = layout })
                { model | toast = Just { text = \t -> t.layoutDetected (Layout.name layout), remaining = toastMilliseconds } }

        Nothing ->
            ( model, Cmd.none )


saveSettings : (Settings -> Settings) -> Model -> ( Model, Cmd Msg )
saveSettings change model =
    let
        settings =
            change model.settings
    in
    ( { model | settings = settings }, Ports.saveSettings (Settings.encode settings) )


playSound : Model -> String -> Cmd Msg
playSound model name =
    if model.settings.soundOn then
        Ports.playSound name

    else
        Cmd.none


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


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen next ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            next model
    in
    ( nextModel, Cmd.batch [ cmd, nextCmd ] )



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

        DrinkMe ->
            DrinkMeChapter DrinkMe.init

        PoolOfTears ->
            PoolChapter Pool.init

        CaterpillarsAdvice ->
            CaterpillarChapter Caterpillar.init


chapterId : Chapter -> ChapterId
chapterId chapter =
    case chapter of
        RabbitHoleChapter _ ->
            RabbitHole

        HallChapter _ ->
            HallOfDoors

        DrinkMeChapter _ ->
            DrinkMe

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

        DrinkMeChapter model ->
            run DrinkMe.update DrinkMe.KeyPressed DrinkMe.Frame DrinkMeChapter model

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

        DrinkMeChapter model ->
            DrinkMe.view ctx model

        PoolChapter model ->
            Pool.view ctx model

        CaterpillarChapter model ->
            Caterpillar.view ctx model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onAnimationFrameDelta Frame
        ]


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
                        [ "j", "k" ]

                Results id score ->
                    cardScreen
                        (Ui.Results.view
                            { texts = texts
                            , chapter = id
                            , score = score
                            , onNext =
                                case ChapterId.next id of
                                    Just next ->
                                        StartedChapter next

                                    Nothing ->
                                        FinishedGame
                            , onReplay = StartedChapter id
                            , onMenu = OpenedMenu
                            }
                        )
                        (Html.p [ A.class "lesson" ] [ Html.text (texts.chapter id).lesson ])
                        []

                TheEnd ->
                    cardScreen
                        (Ui.TheEnd.view { texts = texts, totalTarts = Progress.total model.progress, onMenu = OpenedMenu })
                        (Html.p [ A.class "big" ] [ Html.text texts.theEnd.teaser ])
                        []
    in
    Html.div [ A.class "app" ]
        [ Chrome.header
            { texts = texts
            , settings = model.settings
            , onLanguage = ChoseLanguage
            , onLayout = ChoseLayout
            , onToggleSound = ToggledSound
            , onMenu = OpenedMenu
            }
        , Html.main_ [ A.class "stage" ]
            [ screen.stage
            , case model.toast of
                Just toast ->
                    Html.div [ A.class "toast", A.attribute "role" "status" ] [ Html.text (toast.text texts) ]

                Nothing ->
                    Html.text ""
            ]
        , Html.section [ A.class "panel", A.attribute "aria-live" "polite" ] [ screen.panel ]
        , Chrome.statusLine screen.status
        , Keyboard.View.view
            { texts = texts
            , layout = model.settings.layout
            , focus = screen.keyboard
            , flash = Maybe.map (\f -> { key = f.key, ok = f.ok }) model.flash
            }
        , Html.nav [ A.class "footer-links" ]
            [ Html.a [ A.href Project.pixelEditorUrl, A.target "_blank", A.rel "noopener" ] [ Html.text texts.drawPictures ]
            , Html.a [ A.href (Project.proposeEditUrl (screenName model)), A.target "_blank", A.rel "noopener" ] [ Html.text texts.suggestChange ]
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
cardScreen : Html Msg -> Html Msg -> List String -> Contract.View Msg
cardScreen card panel helpers =
    { stage =
        Html.div [ A.class "stage-scene" ]
            [ Hall.backdrop
            , Html.div [ A.class "overlay" ] [ card ]
            ]
    , panel = panel
    , status = { mode = "NOR", file = "wonderland.txt", pendingKeys = "", position = "1:1" }
    , keyboard = { target = Nothing, helpers = helpers }
    }
