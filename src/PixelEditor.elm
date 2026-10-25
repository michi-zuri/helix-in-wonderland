module PixelEditor exposing (main)

{-| A small pixel editor for proposing new art for the game.

It starts from the game's own pictures (`Pixel.Sprites.catalogue`), so it
always shows what is really in the game. Paint with the mouse, change
colours, then send the result as a GitHub issue with one click. The issue
form lives in `.github/ISSUE_TEMPLATE/pixel-art.yml`; ART.md explains the
whole process.

Built into `dist/site/pixel-editor.html` by `npm run build`.

-}

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import Pixel.Sprite as Sprite
import Pixel.Sprites as Sprites
import Project
import Svg
import Svg.Attributes as SA
import Url


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( load firstPicture, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


transparent : Char
transparent =
    '.'



-- MODEL


type alias Model =
    { picture : String
    , pixels : Array (Array Char)
    , palette : List ( Char, String )
    , brush : Char
    , painting : Bool
    , changed : Bool
    }


firstPicture : String
firstPicture =
    "tart"


load : String -> Model
load name =
    let
        sprite =
            Sprites.catalogue
                |> List.filter (\entry -> entry.name == name)
                |> List.head
                |> Maybe.map .sprite
                |> Maybe.withDefault Sprites.tart

        palette =
            Sprite.palette sprite
    in
    { picture = name
    , pixels = Sprite.rows sprite |> List.map (String.toList >> Array.fromList) |> Array.fromList
    , palette = palette
    , brush = palette |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault transparent
    , painting = False
    , changed = False
    }



-- UPDATE


type Msg
    = ChosePicture String
    | ChoseBrush Char
    | PressedPixel Int Int
    | EnteredPixel Int Int
    | Released
    | ChangedColour Char String
    | AddedColour
    | ResetPicture


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChosePicture name ->
            load name

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
                , changed = True
            }

        AddedColour ->
            case freeLetter model.palette of
                Just letter ->
                    { model | palette = model.palette ++ [ ( letter, "#ffffff" ) ], brush = letter, changed = True }

                Nothing ->
                    model

        ResetPicture ->
            load model.picture


paint : Int -> Int -> Model -> Model
paint x y model =
    case Array.get y model.pixels of
        Just row ->
            { model | pixels = Array.set y (Array.set x model.brush row) model.pixels, changed = True }

        Nothing ->
            model


{-| A letter not used yet, for a new colour.
-}
freeLetter : List ( Char, String ) -> Maybe Char
freeLetter palette =
    String.toList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> List.filter (\letter -> not (List.any (\( used, _ ) -> used == letter) palette))
        |> List.head



-- TEXT FOR THE PROPOSAL


rowsText : Model -> String
rowsText model =
    model.pixels
        |> Array.toList
        |> List.map (Array.toList >> String.fromList)
        |> String.join "\n"


coloursText : Model -> String
coloursText model =
    model.palette
        |> List.map (\( letter, colour ) -> String.fromChar letter ++ " " ++ colour)
        |> String.join "\n"


{-| Opens GitHub's "new issue" page with the pixel-art form already filled
in (GitHub fills form fields from the link's query parameters).
-}
proposalLink : Model -> String
proposalLink model =
    Project.newIssueUrl
        ++ "?template=pixel-art.yml"
        ++ "&title="
        ++ Url.percentEncode ("Pixel art: " ++ model.picture)
        ++ "&picture="
        ++ Url.percentEncode model.picture
        ++ "&rows="
        ++ Url.percentEncode (rowsText model)
        ++ "&colours="
        ++ Url.percentEncode (coloursText model)



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
            [ viewGrid model
            , Html.div [ A.class "side" ]
                [ Html.h2 [] [ Html.text "Colours · Farben" ]
                , viewPalette model
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
        , Html.pre [ A.class "text" ] [ Html.text (coloursText model ++ "\n\n" ++ rowsText model) ]
        , Html.nav [ A.class "links" ]
            [ Html.a [ A.href Project.gameUrl ] [ Html.text "← Back to the game · Zurück zum Spiel" ]
            , Html.a [ A.href (Project.proposeEditUrl "Pixel editor"), A.target "_blank", A.rel "noopener" ]
                [ Html.text "Suggest a change to this page · Änderung für diese Seite vorschlagen" ]
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    let
        width =
            Array.get 0 model.pixels |> Maybe.map Array.length |> Maybe.withDefault 0
    in
    Html.div
        [ A.class "grid"
        , A.style "grid-template-columns" ("repeat(" ++ String.fromInt width ++ ", 1fr)")
        , A.style "aspect-ratio" (String.fromInt width ++ " / " ++ String.fromInt (Array.length model.pixels))
        ]
        (model.pixels
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


viewPalette : Model -> Html Msg
viewPalette model =
    Html.div [ A.class "palette" ]
        (List.map
            (\( letter, colour ) ->
                Html.div [ A.classList [ ( "swatch", True ), ( "chosen", letter == model.brush ) ] ]
                    [ Html.button [ E.onClick (ChoseBrush letter), A.style "background" colour ] [ Html.text (String.fromChar letter) ]
                    , Html.input [ A.type_ "color", A.value colour, E.onInput (ChangedColour letter) ] []
                    ]
            )
            model.palette
            ++ [ Html.div [ A.classList [ ( "swatch", True ), ( "chosen", model.brush == transparent ) ] ]
                    [ Html.button [ A.class "eraser", E.onClick (ChoseBrush transparent) ] [ Html.text "." ]
                    , Html.span [] [ Html.text "eraser · Radierer" ]
                    ]
               , Html.button [ A.class "add", E.onClick AddedColour ] [ Html.text "+ new colour · neue Farbe" ]
               ]
        )


viewPreview : Model -> Html Msg
viewPreview model =
    let
        sprite =
            Sprite.fromRows model.palette (String.lines (rowsText model))

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
