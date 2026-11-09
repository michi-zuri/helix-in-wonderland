module ProgramFlowTest exposing (suite)

{-| The whole game as one program: the title menu, starting chapters,
results, the end card, settings, saved progress and fullscreen. Keys, frames
and the browser's fullscreen arrive through simulated subscriptions; saving
and sounds leave through simulated ports. The chapters' own rules are tested
in their playthrough tests; these tests check how the shell ties them
together, which only a browser could check before.
-}

import App
import Chapter.RabbitHole as RabbitHole
import Expect
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import SimulatedEffect.Sub
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, text)


type alias Game =
    ProgramTest App.Model App.Msg (List App.Command)


{-| Start the game as a browser would, with whatever was saved before.
-}
startWith : List ( String, Encode.Value ) -> Game
startWith saved =
    ProgramTest.createElement
        { init = App.init
        , update = App.update
        , view = App.view
        }
        |> ProgramTest.withSimulatedEffects simulateCommands
        |> ProgramTest.withSimulatedSubscriptions simulateSubscriptions
        |> ProgramTest.start
            (Encode.object
                ([ ( "browserLanguage", Encode.string "en-US" )
                 , ( "seed", Encode.int 4 )
                 ]
                    ++ saved
                )
            )


newPlayer : Game
newPlayer =
    startWith []


simulateCommands : List App.Command -> ProgramTest.SimulatedEffect App.Msg
simulateCommands commands =
    commands
        |> List.map
            (\command ->
                case command of
                    App.SaveSettings value ->
                        SimulatedEffect.Ports.send "saveSettings" value

                    App.SaveProgress value ->
                        SimulatedEffect.Ports.send "saveProgress" value

                    App.Sound name ->
                        SimulatedEffect.Ports.send "playSound" (Encode.string name)

                    App.SetFullscreen on ->
                        SimulatedEffect.Ports.send "setFullscreen" (Encode.bool on)
            )
        |> SimulatedEffect.Cmd.batch


{-| The same subscriptions as `Main`, as simulated ports: "keydown" for the
keyboard, "frame" for animation frames, "fullscreenChanged" for the browser.
-}
simulateSubscriptions : App.Model -> ProgramTest.SimulatedSub App.Msg
simulateSubscriptions model =
    SimulatedEffect.Sub.batch
        [ SimulatedEffect.Ports.subscribe "keydown" App.keyDecoder identity
        , SimulatedEffect.Ports.subscribe "fullscreenChanged" Decode.bool App.fullscreenChanged
        , if App.needsFrames model then
            SimulatedEffect.Ports.subscribe "frame" Decode.float App.frame

          else
            SimulatedEffect.Sub.none
        ]



-- PLAYING


keyEvent : { key : String, ctrl : Bool } -> Encode.Value
keyEvent { key, ctrl } =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "code", Encode.string "" )
        , ( "ctrlKey", Encode.bool ctrl )
        , ( "altKey", Encode.bool False )
        , ( "metaKey", Encode.bool False )
        ]


press : List String -> Game -> Game
press keys game =
    List.foldl (\key -> ProgramTest.simulateIncomingPort "keydown" (keyEvent { key = key, ctrl = False })) game keys


{-| Every character of `text` as a key.
-}
typeIn : String -> Game -> Game
typeIn text =
    press (List.map String.fromChar (String.toList text))


wait : Float -> Game -> Game
wait milliseconds game =
    List.foldl (\_ -> ProgramTest.simulateIncomingPort "frame" (Encode.float 50)) game (List.range 1 (round (milliseconds / 50)))


readStory : Game -> Game
readStory =
    press (List.repeat 12 "Enter")


portValues : String -> Decode.Decoder a -> (List a -> Expect.Expectation) -> Game -> Expect.Expectation
portValues =
    ProgramTest.expectOutgoingPortValues


suite : Test
suite =
    describe "The whole game"
        [ describe "the title menu"
            [ test "a new player starts at chapter 1; chapter 2 is still locked" <|
                \_ ->
                    newPlayer
                        |> press [ "j", "Enter" ]
                        |> Expect.all
                            [ ProgramTest.expectViewHas [ text "Finish the chapter before this one first." ]
                            , portValues "playSound" Decode.string (Expect.equal [ "error" ])
                            ]
            , test "Enter on chapter 1 starts the fall" <|
                \_ ->
                    newPlayer
                        |> press [ "Enter" ]
                        |> ProgramTest.expectViewHas [ text "01_rabbit_hole.txt" ]
            , test "the grown-ups button opens every chapter, and is saved" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.clickButton "Unlock all chapters (for grown-ups)"
                        |> press [ "j", "Enter" ]
                        |> Expect.all
                            [ ProgramTest.expectViewHas [ text "02_hall_of_doors.txt" ]
                            , portValues "saveSettings" (Decode.field "unlockAll" Decode.bool) (Expect.equal [ True ])
                            ]
            , test "a saved game with tarts in the hall opens chapter 3, the little door" <|
                \_ ->
                    startWith [ ( "progress", Encode.object [ ( "hall-of-doors", Encode.int 2 ) ] ) ]
                        |> press [ "j", "j", "Enter" ]
                        |> ProgramTest.expectViewHas [ text "03_little_door.txt" ]
            , test "... but chapter 4 stays locked" <|
                \_ ->
                    startWith [ ( "progress", Encode.object [ ( "hall-of-doors", Encode.int 2 ) ] ) ]
                        |> press [ "j", "j", "j", "Enter" ]
                        |> ProgramTest.expectViewHas [ text "Finish the chapter before this one first." ]
            ]
        , test "finishing the last chapter: the results card, saved progress, then the end card" <|
            \_ ->
                newPlayer
                    |> ProgramTest.clickButton "Unlock all chapters (for grown-ups)"
                    |> press [ "k", "Enter" ]
                    |> readStory
                    |> press [ "1" ]
                    |> typeIn "9liyou"
                    |> press [ "Escape" ]
                    |> typeIn ("j" ++ String.repeat 40 "h" ++ "lllihardly")
                    |> press [ "Escape" ]
                    |> typeIn ("j" ++ String.repeat 40 "h" ++ String.repeat 11 "l" ++ "itemper")
                    |> press [ "Escape" ]
                    -- The closing story has two lines: Enter shows each in full, Enter goes on.
                    |> press (List.repeat 4 "Enter")
                    |> Expect.all
                        [ ProgramTest.expectViewHas [ text "Chapter complete!" ]
                        , portValues "saveProgress" (Decode.field "caterpillar" Decode.int) (Expect.equal [ 1 ])
                        , press [ "Enter" ] >> ProgramTest.expectViewHas [ text "The end, for now" ]
                        ]
        , test "chapter 3: open the little door's locks, then Next leads into Drink Me" <|
            \_ ->
                startWith [ ( "progress", Encode.object [ ( "hall-of-doors", Encode.int 1 ) ] ) ]
                    |> press [ "j", "j", "Enter" ]
                    |> readStory
                    |> press [ "1" ]
                    -- Each hint dialog line takes two Enters: one shows it in full, one goes on (#93).
                    |> press [ "Enter", "Enter" ]
                    |> typeIn "6li"
                    -- The key is in, and the Esc fallback: two lines.
                    |> press [ "Enter", "Enter", "Enter", "Enter" ]
                    |> typeIn "had"
                    |> press [ "Escape" ]
                    |> typeIn "jiglad"
                    |> press [ "Escape" ]
                    |> typeIn "j2lihalf"
                    |> press [ "Escape" ]
                    |> typeIn "j11l"
                    -- A wrong pin.
                    |> press [ "Enter", "Enter" ]
                    |> typeIn "cl"
                    |> press [ "Escape" ]
                    |> press (List.repeat 4 "Enter")
                    |> ProgramTest.ensureViewHas [ text "Chapter complete!" ]
                    |> press [ "Enter" ]
                    |> ProgramTest.expectViewHas [ text "04_drink_me.txt" ]
        , describe "fullscreen"
            [ test "entering fullscreen asks the browser, then asks for the escape keys and saves them" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.clickButton "⛶ Fullscreen"
                        |> ProgramTest.ensureViewHas [ text "Your escape keys" ]
                        |> press [ ";", ";" ]
                        |> Expect.all
                            [ portValues "setFullscreen" Decode.bool (Expect.equal [ True ])
                            , portValues "saveSettings" (Decode.field "escapeKeys" Decode.string) (Expect.equal [ ";;" ])
                            , ProgramTest.expectViewHas [ text "Your escape keys: ;;" ]
                            ]
            , test "leaving the browser's fullscreen (holding Esc, F11) leaves the fullscreen layout" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.clickButton "⛶ Fullscreen"
                        |> press [ ";", ";" ]
                        |> ProgramTest.ensureViewHas [ class "fullscreen" ]
                        |> ProgramTest.simulateIncomingPort "fullscreenChanged" (Encode.bool False)
                        |> ProgramTest.expectViewHasNot [ class "fullscreen" ]
            , test "the escape keys work like Esc inside a chapter" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.clickButton "⛶ Fullscreen"
                        |> press [ ";", ";" ]
                        |> ProgramTest.clickButton "Unlock all chapters (for grown-ups)"
                        |> press [ "k", "Enter" ]
                        |> readStory
                        |> press [ "1", "i", "y" ]
                        |> ProgramTest.ensureViewHas [ text "INSERT" ]
                        |> press [ ";", ";" ]
                        |> ProgramTest.expectViewHas [ text "NORMAL" ]
            ]
        , test "Ctrl+[ works as Esc" <|
            \_ ->
                newPlayer
                    |> ProgramTest.clickButton "Unlock all chapters (for grown-ups)"
                    |> press [ "k", "Enter" ]
                    |> readStory
                    |> press [ "1", "i", "y" ]
                    |> ProgramTest.ensureViewHas [ text "INSERT" ]
                    |> ProgramTest.simulateIncomingPort "keydown" (keyEvent { key = "[", ctrl = True })
                    |> ProgramTest.expectViewHas [ text "NORMAL" ]
        , describe "settings"
            [ test "German: the menu changes language, and the choice is saved" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.within (Query.find [ attribute (Html.Attributes.attribute "aria-label" "Language") ]) (ProgramTest.clickButton "DE")
                        |> Expect.all
                            [ ProgramTest.expectViewHas [ text "Hinunter in den Kaninchenbau" ]
                            , portValues "saveSettings" (Decode.field "language" Decode.string) (Expect.equal [ "DE" ])
                            ]
            , test "with the sound off, a locked chapter makes no sound" <|
                \_ ->
                    newPlayer
                        |> ProgramTest.clickButton "Sound on"
                        |> press [ "j", "Enter" ]
                        |> portValues "playSound" Decode.string (Expect.equal [])
            ]
        , test "frames run a chapter: a jar rises, and catching it brings Alice one step down" <|
            \_ ->
                newPlayer
                    |> press [ "Enter" ]
                    |> readStory
                    |> press [ "1" ]
                    |> ProgramTest.ensureViewHas [ text ("0/" ++ String.fromInt RabbitHole.basicJars) ]
                    -- How to catch, how to let go, the first stage: the fall waits until they are read.
                    |> readStory
                    |> wait 1000
                    -- The first stage is the left index finger: F.
                    |> press [ "f" ]
                    |> ProgramTest.expectViewHas [ text ("1/" ++ String.fromInt RabbitHole.basicJars) ]
        ]
