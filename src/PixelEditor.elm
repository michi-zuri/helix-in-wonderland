module PixelEditor exposing (Model, Msg(..), main, rowsText, start, update)

{-| A small pixel editor for proposing new art for the game.

It starts from the game's own pictures (`Pixel.Sprites.catalogue`), so it
always shows what is really in the game. Paint with the mouse, change
colours and their letters, make the picture bigger when an idea needs room
(`Pixel.Canvas`, #134), give it several frames that loop in the preview
(#136), then send the result as a GitHub issue with one click. The issue
form lives in `.github/ISSUE_TEMPLATE/pixel-art.yml`; ART.md explains the
whole process.

Built into `dist/site/pixel-editor.html` by `npm run build`.

-}

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed
import Json.Decode as Decode
import Pixel.Canvas as Canvas exposing (Side(..))
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Project
import Svg
import Svg.Attributes as SA
import Url


{-| The page passes where the site and its repository are, so links work on
any copy of the game (see `Project`).
-}
main : Program Decode.Value Model Msg
main =
    Browser.element
        { init =
            \flags ->
                ( start (Decode.decodeValue (Decode.field "links" Project.decoder) flags |> Result.withDefault Project.fallback)
                , Cmd.none
                )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }


transparent : Char
transparent =
    '.'


{-| The preview's frame rate can be set between these, in frames per second.
-}
slowest : Float
slowest =
    1


fastest : Float
fastest =
    12



-- MODEL


type alias Picture =
    Array (Array Char)


{-| `frames` all have the same size and share the palette; `frame` is the
one being painted. `letterEdits` counts attempts to change a letter, so the
letter fields are drawn afresh even when a change is refused.
-}
type alias Model =
    { links : Project.Links
    , picture : String
    , frames : Array Picture
    , frame : Int
    , framesPerSecond : Float
    , palette : List ( Char, String )
    , brush : Char
    , painting : Bool
    , clock : Float
    , letterEdits : Int
    }


firstPicture : String
firstPicture =
    "tart"


start : Project.Links -> Model
start links =
    load links firstPicture


load : Project.Links -> String -> Model
load links name =
    let
        entry =
            Sprites.catalogue
                |> List.filter (\candidate -> candidate.name == name)
                |> List.head
                |> Maybe.withDefault { name = name, frames = [ Sprites.tart ], framesPerSecond = 4 }

        palette =
            entry.frames
                |> List.concatMap Sprite.palette
                |> List.foldl
                    (\( letter, colour ) found ->
                        if List.any (\( known, _ ) -> known == letter) found then
                            found

                        else
                            found ++ [ ( letter, colour ) ]
                    )
                    []

        toPicture sprite =
            Sprite.rows sprite |> List.map (String.toList >> Array.fromList) |> Array.fromList
    in
    { links = links
    , picture = name
    , frames = Array.fromList (List.map toPicture entry.frames)
    , frame = 0
    , framesPerSecond = entry.framesPerSecond
    , palette = palette
    , brush = palette |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault transparent
    , painting = False
    , clock = 0
    , letterEdits = 0
    }


currentPicture : Model -> Picture
currentPicture model =
    Array.get model.frame model.frames |> Maybe.withDefault Array.empty


frameRows : Picture -> List String
frameRows picture =
    picture |> Array.toList |> List.map (Array.toList >> String.fromList)



-- UPDATE


type Msg
    = ChosePicture String
    | ChoseBrush Char
    | PressedPixel Int Int
    | EnteredPixel Int Int
    | Released
    | ChangedColour Char String
    | ChangedLetter Char String
    | AddedColour
    | ResetPicture
    | Grew Side
    | ChoseFrame Int
    | AddedFrame
    | RemovedFrame
    | ChangedSpeed String
    | Ticked Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChosePicture name ->
            load model.links name

        ChoseBrush letter ->
            { model | brush = letter }

        PressedPixel x y ->
            paint x y { model | painting = True }

        EnteredPixel x y ->
            if model.painting then
                paint x y model

            else
                model

        Released ->
            { model | painting = False }

        ChangedColour letter colour ->
            { model
                | palette =
                    List.map
                        (\( l, c ) ->
                            if l == letter then
                                ( l, colour )

                            else
                                ( l, c )
                        )
                        model.palette
            }

        ChangedLetter old typed ->
            renameLetter old typed { model | letterEdits = model.letterEdits + 1 }

        AddedColour ->
            case freeLetter model.palette of
                Just letter ->
                    { model | palette = model.palette ++ [ ( letter, "#ffffff" ) ], brush = letter }

                Nothing ->
                    model

        ResetPicture ->
            load model.links model.picture

        Grew side ->
            { model | frames = Array.map (Canvas.grow side) model.frames }

        ChoseFrame index ->
            { model | frame = clamp 0 (Array.length model.frames - 1) index }

        AddedFrame ->
            -- A copy of the frame being painted, right after it.
            let
                at =
                    model.frame + 1
            in
            { model
                | frames =
                    Array.append
                        (Array.push (currentPicture model) (Array.slice 0 at model.frames))
                        (Array.slice at (Array.length model.frames) model.frames)
                , frame = at
            }

        RemovedFrame ->
            if Array.length model.frames <= 1 then
                model

            else
                { model
                    | frames =
                        Array.append
                            (Array.slice 0 model.frame model.frames)
                            (Array.slice (model.frame + 1) (Array.length model.frames) model.frames)
                    , frame = max 0 (model.frame - 1)
                }

        ChangedSpeed text ->
            case String.toFloat text of
                Just speed ->
                    { model | framesPerSecond = clamp slowest fastest speed }

                Nothing ->
                    model

        Ticked milliseconds ->
            { model | clock = model.clock + milliseconds }


paint : Int -> Int -> Model -> Model
paint x y model =
    let
        picture =
            currentPicture model
    in
    case Array.get y picture of
        Just row ->
            { model | frames = Array.set model.frame (Array.set y (Array.set x model.brush row) picture) model.frames }

        Nothing ->
            model


{-| A colour gets a new letter, everywhere in every frame. Only a letter or
digit not used yet is taken; anything else is ignored. The field still
holds the old letter while typing, so the new one is the character that
isn't the old one.
-}
renameLetter : Char -> String -> Model -> Model
renameLetter old typed model =
    let
        new =
            String.toList typed |> List.filter ((/=) old) |> List.reverse |> List.head

        isUsed letter =
            List.any (\( known, _ ) -> known == letter) model.palette

        swap letter =
            if letter == old then
                Maybe.withDefault old new

            else
                letter
    in
    case new of
        Just letter ->
            if Char.isAlphaNum letter && not (isUsed letter) then
                { model
                    | palette = List.map (\( l, c ) -> ( swap l, c )) model.palette
                    , frames = Array.map (Array.map (Array.map swap)) model.frames
                    , brush = swap model.brush
                }

            else
                model

        Nothing ->
            model


{-| A letter not used yet, for a new colour.
-}
freeLetter : List ( Char, String ) -> Maybe Char
freeLetter palette =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> List.filter (\letter -> not (List.any (\( used, _ ) -> used == letter) palette))
        |> List.head


subscriptions : Model -> Sub Msg
subscriptions model =
    if Array.length model.frames > 1 then
        Browser.Events.onAnimationFrameDelta Ticked

    else
        Sub.none



-- TEXT FOR THE PROPOSAL


{-| The frames as sent: trimmed together, so empty edges go only where
every frame is empty (#134, #136).
-}
sentFrames : Model -> List (List String)
sentFrames model =
    model.frames |> Array.toList |> List.map frameRows |> Canvas.trimFrames


{-| The picture as sent: each frame's rows, a blank line between frames.
-}
rowsText : Model -> String
rowsText model =
    sentFrames model |> List.map (String.join "\n") |> String.join "\n\n"


coloursText : Model -> String
coloursText model =
    model.palette
        |> List.map (\( letter, colour ) -> String.fromChar letter ++ " " ++ colour)
        |> String.join "\n"


speedText : Model -> String
speedText model =
    String.fromFloat model.framesPerSecond


{-| Opens GitHub's "new issue" page with the pixel-art form already filled
in (GitHub fills form fields from the link's query parameters).
-}
proposalLink : Model -> String
proposalLink model =
    Project.newIssueUrl model.links
        ++ "?template=pixel-art.yml"
        ++ "&title="
        ++ Url.percentEncode ("Pixel art: " ++ model.picture)
        ++ "&picture="
        ++ Url.percentEncode model.picture
        ++ "&rows="
        ++ Url.percentEncode (rowsText model)
        ++ "&colours="
        ++ Url.percentEncode (coloursText model)
        ++ (if Array.length model.frames > 1 then
                "&fps=" ++ Url.percentEncode (speedText model)

            else
                ""
           )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ A.class "editor-page", E.onMouseUp Released, E.onMouseLeave Released ]
        [ Html.h1 [] [ Html.text "Pixel editor · Pixel-Editor" ]
        , Html.p [ A.class "intro" ]
            [ Html.text "Pick a picture from the game, paint over it, and send your idea. · Wähle ein Bild aus dem Spiel, mal darüber und schick deine Idee." ]
        , Html.div [ A.class "pictures" ]
            (List.map
                (\entry ->
                    Html.button
                        [ A.classList [ ( "picture", True ), ( "chosen", entry.name == model.picture ) ]
                        , E.onClick (ChosePicture entry.name)
                        ]
                        [ Html.text entry.name ]
                )
                Sprites.catalogue
            )
        , Html.div [ A.class "workspace" ]
            [ viewCanvas model
            , Html.div [ A.class "side" ]
                [ Html.h2 [] [ Html.text "Colours · Farben" ]
                , viewPalette model
                , Html.h2 [] [ Html.text "Frames · Bilder" ]
                , viewFrames model
                , Html.h2 [] [ Html.text "Preview · Vorschau" ]
                , viewPreview model
                ]
            ]
        , Html.div [ A.class "actions" ]
            [ Html.a [ A.class "send", A.href (proposalLink model), A.target "_blank", A.rel "noopener" ]
                [ Html.text "Send as a proposal on GitHub · Als Vorschlag auf GitHub schicken" ]
            , Html.button [ E.onClick ResetPicture ] [ Html.text "Start over · Neu anfangen" ]
            ]
        , Html.h2 [] [ Html.text "Your picture as text · Dein Bild als Text" ]
        , Html.pre [ A.class "text" ]
            [ Html.text
                (coloursText model
                    ++ "\n\n"
                    ++ rowsText model
                    ++ (if Array.length model.frames > 1 then
                            "\n\nframes per second · Bilder pro Sekunde: " ++ speedText model

                        else
                            ""
                       )
                )
            ]
        , Html.nav [ A.class "links" ]
            [ Html.a [ A.href (Project.gameUrl model.links) ] [ Html.text "← Back to the game · Zurück zum Spiel" ]
            , Html.a [ A.href (Project.proposeEditUrl model.links "Pixel editor"), A.target "_blank", A.rel "noopener" ]
                [ Html.text "Suggest a change to this page · Änderung für diese Seite vorschlagen" ]
            ]
        ]


{-| The grid with a bar on each side, shaped like the row or column it adds
(#136). The sizes come from the CSS variables `--cols` and `--rows`, which
`Html.Attributes.style` can't set, so they go in the style attribute.
-}
viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        picture =
            currentPicture model

        width =
            Array.get 0 picture |> Maybe.map Array.length |> Maybe.withDefault 0

        bar side name label =
            Html.button
                [ A.class ("grow-bar " ++ name)
                , E.onClick (Grew side)
                , A.title label
                , A.attribute "aria-label" label
                ]
                [ Html.text "+" ]
    in
    Html.div [ A.class "canvas-room" ]
        [ Html.div
            [ A.class "canvas"
            , A.attribute "style" ("--cols:" ++ String.fromInt width ++ ";--rows:" ++ String.fromInt (Array.length picture))
            ]
            [ bar Top "top" "Add a row on top · Zeile oben dazu"
            , bar Left "left" "Add a column on the left · Spalte links dazu"
            , viewGrid width picture model
            , bar Right "right" "Add a column on the right · Spalte rechts dazu"
            , bar Bottom "bottom" "Add a row below · Zeile unten dazu"
            ]
        ]


viewGrid : Int -> Picture -> Model -> Html Msg
viewGrid width picture model =
    Html.div
        [ A.class "grid"
        , A.style "grid-template-columns" ("repeat(" ++ String.fromInt width ++ ", 1fr)")
        , A.style "grid-template-rows" ("repeat(" ++ String.fromInt (Array.length picture) ++ ", 1fr)")
        ]
        (picture
            |> Array.toIndexedList
            |> List.concatMap
                (\( y, row ) ->
                    row
                        |> Array.toIndexedList
                        |> List.map (\( x, letter ) -> viewPixel model x y letter)
                )
        )


viewPixel : Model -> Int -> Int -> Char -> Html Msg
viewPixel model x y letter =
    Html.div
        [ A.class "pixel"
        , A.style "background" (colourOf model letter)
        , E.preventDefaultOn "mousedown" (Decode.succeed ( PressedPixel x y, True ))
        , E.onMouseEnter (EnteredPixel x y)
        ]
        []


{-| Each colour: a button to paint with it, its letter (which can be
changed), and its colour.
-}
viewPalette : Model -> Html Msg
viewPalette model =
    Html.Keyed.node "div"
        [ A.class "palette" ]
        (List.map
            (\( letter, colour ) ->
                ( String.fromChar letter ++ String.fromInt model.letterEdits
                , Html.div [ A.classList [ ( "swatch", True ), ( "chosen", letter == model.brush ) ] ]
                    [ Html.button [ E.onClick (ChoseBrush letter), A.style "background" colour, A.attribute "aria-label" ("Paint with " ++ String.fromChar letter) ] []
                    , Html.input
                        [ A.class "letter"
                        , A.value (String.fromChar letter)
                        , E.onInput (ChangedLetter letter)
                        , A.title "Letter · Buchstabe"
                        , A.attribute "aria-label" "Letter · Buchstabe"
                        , A.attribute "autocomplete" "off"
                        , A.spellcheck False
                        ]
                        []
                    , Html.input [ A.type_ "color", A.value colour, E.onInput (ChangedColour letter) ] []
                    ]
                )
            )
            model.palette
            ++ [ ( "eraser"
                 , Html.div [ A.classList [ ( "swatch", True ), ( "chosen", model.brush == transparent ) ] ]
                    [ Html.button [ A.class "eraser", E.onClick (ChoseBrush transparent) ] [ Html.text "." ]
                    , Html.span [] [ Html.text "eraser · Radierer" ]
                    ]
                 )
               , ( "add", Html.button [ A.class "add", E.onClick AddedColour ] [ Html.text "+ new colour · neue Farbe" ] )
               ]
        )


{-| Which frame is painted, a frame more or less, and how fast they loop.
-}
viewFrames : Model -> Html Msg
viewFrames model =
    let
        count =
            Array.length model.frames
    in
    Html.div [ A.class "frames" ]
        [ Html.div [ A.class "frame-buttons" ]
            (List.map
                (\index ->
                    Html.button
                        [ A.classList [ ( "frame", True ), ( "chosen", index == model.frame ) ]
                        , E.onClick (ChoseFrame index)
                        ]
                        [ Html.text (String.fromInt (index + 1)) ]
                )
                (List.range 0 (count - 1))
                ++ [ Html.button [ E.onClick AddedFrame, A.title "Add a frame (a copy of this one) · Bild dazu (eine Kopie von diesem)" ] [ Html.text "+" ]
                   , Html.button [ E.onClick RemovedFrame, A.disabled (count <= 1), A.title "Remove this frame · Dieses Bild entfernen" ] [ Html.text "×" ]
                   ]
            )
        , if count > 1 then
            Html.label [ A.class "speed" ]
                [ Html.input
                    [ A.type_ "range"
                    , A.min (String.fromFloat slowest)
                    , A.max (String.fromFloat fastest)
                    , A.step "1"
                    , A.value (speedText model)
                    , E.onInput ChangedSpeed
                    ]
                    []
                , Html.span [] [ Html.text (speedText model ++ " per second · pro Sekunde") ]
                ]

          else
            Html.text ""
        ]


{-| The picture about as big as in the game, as it will be sent; several
frames take turns.
-}
viewPreview : Model -> Html Msg
viewPreview model =
    let
        frames =
            sentFrames model

        shown =
            modBy (max 1 (List.length frames)) (floor (model.clock * model.framesPerSecond / 1000))

        sprite =
            Sprite.fromRows model.palette (List.drop shown frames |> List.head |> Maybe.withDefault [])

        { width, height } =
            Sprite.size sprite
    in
    Html.div [ A.class "preview" ]
        [ Svg.svg
            [ SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
            , SA.width (String.fromInt (width * 4))
            , SA.height (String.fromInt (height * 4))
            , SA.shapeRendering "crispEdges"
            ]
            [ Sprite.view sprite ]
        ]


colourOf : Model -> Char -> String
colourOf model letter =
    model.palette
        |> List.filter (\( l, _ ) -> l == letter)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault "transparent"
