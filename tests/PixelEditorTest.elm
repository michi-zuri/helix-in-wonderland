module PixelEditorTest exposing (suite)

{-| The pixel editor's colours, letters and frames (#136).
-}

import Array
import Expect
import Pixel.Canvas as Canvas exposing (Side(..))
import PixelEditor exposing (Msg(..))
import Project
import Test exposing (Test, describe, test)


steps : List Msg -> PixelEditor.Model
steps msgs =
    List.foldl PixelEditor.update (PixelEditor.start Project.fallback) msgs


tart : List Msg
tart =
    [ ChosePicture "tart" ]


suite : Test
suite =
    describe "pixel editor"
        [ describe "a colour's letter can be changed"
            [ test "everywhere in the picture and in the colours" <|
                \_ ->
                    steps (tart ++ [ ChangedLetter 'c' "cz" ])
                        |> (\model -> ( List.map Tuple.first model.palette |> List.member 'z', String.contains "c" (PixelEditor.rowsText model), String.contains "z" (PixelEditor.rowsText model) ))
                        |> Expect.equal ( True, False, True )
            , test "a letter already used is refused" <|
                \_ ->
                    steps (tart ++ [ ChangedLetter 'c' "cj" ])
                        |> PixelEditor.rowsText
                        |> Expect.equal (PixelEditor.rowsText (steps tart))
            , test "only letters and digits: a dot or a space is refused" <|
                \_ ->
                    steps (tart ++ [ ChangedLetter 'c' "c.", ChangedLetter 'c' "c " ])
                        |> PixelEditor.rowsText
                        |> Expect.equal (PixelEditor.rowsText (steps tart))
            , test "the brush follows the renamed colour" <|
                \_ ->
                    steps (tart ++ [ ChoseBrush 'c', ChangedLetter 'c' "cz" ])
                        |> .brush
                        |> Expect.equal 'z'
            ]
        , describe "frames"
            [ test "alice walks: two frames, standing and stepping, at the game's speed" <|
                \_ ->
                    steps [ ChosePicture "alice" ]
                        |> (\model -> ( Array.length model.frames, model.framesPerSecond ))
                        |> Expect.equal ( 2, 8 )
            , test "+ adds a copy of the frame being painted, and paints on it" <|
                \_ ->
                    steps (tart ++ [ AddedFrame, PressedPixel 0 0, Released ])
                        |> (\model -> ( Array.length model.frames, model.frame, PixelEditor.rowsText model |> String.split "\n\n" |> List.length ))
                        |> Expect.equal ( 2, 1, 2 )
            , test "× removes a frame, never the last one" <|
                \_ ->
                    steps (tart ++ [ AddedFrame, RemovedFrame, RemovedFrame ])
                        |> .frames
                        |> Array.length
                        |> Expect.equal 1
            , test "the + bars grow every frame" <|
                \_ ->
                    steps [ ChosePicture "alice", Grew Left, Grew Top ]
                        |> .frames
                        |> Array.toList
                        |> List.map (\frame -> ( Array.length frame, Array.get 0 frame |> Maybe.map Array.length ))
                        |> Expect.equal [ ( 17, Just 13 ), ( 17, Just 13 ) ]
            , test "the speed stays between 1 and 12 frames per second" <|
                \_ ->
                    [ steps [ ChangedSpeed "30" ], steps [ ChangedSpeed "0" ] ]
                        |> List.map .framesPerSecond
                        |> Expect.equal [ 12, 1 ]
            ]
        , describe "trimming frames together"
            [ test "an edge is cut only where every frame is empty, so the frames stay aligned" <|
                \_ ->
                    Canvas.trimFrames [ [ "....", ".a..", "...." ], [ "....", "..b.", "...." ] ]
                        |> Expect.equal [ [ "a." ], [ ".b" ] ]
            , test "alice's frames are sent trimmed together: same width" <|
                \_ ->
                    steps [ ChosePicture "alice" ]
                        |> PixelEditor.rowsText
                        |> String.split "\n\n"
                        |> List.map (String.lines >> List.map String.length >> List.maximum)
                        |> Expect.equal [ Just 10, Just 10 ]
            ]
        ]
