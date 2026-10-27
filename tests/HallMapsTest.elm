module HallMapsTest exposing (suite)

{-| The three halls of chapter 2 keep the promises made for them:

  - every hall can be finished, by playing the route with the fewest
    keystrokes through the real chapter;
  - challenge needs all four directions and many turns;
  - mad is a maze whose rooms only doors connect, and its route uses
    several doors.

-}

import Chapter.Contract exposing (Context)
import Chapter.Hall as Hall
import Chapter.Hall.Map as Map exposing (Map, Point)
import Chapter.Hall.Route as Route exposing (Route)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Chapter.Story as Story
import Difficulty exposing (Difficulty(..))
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Set
import Test exposing (Test, describe, test)
import Texts


context : Context
context =
    { texts = Texts.forLanguage English, layout = UsQwerty }


mapOf : Difficulty -> Map
mapOf difficulty =
    Map.parse (Map.plan difficulty)


routeOf : Difficulty -> Route
routeOf difficulty =
    Route.fewest 30 (mapOf difficulty)
        |> Maybe.withDefault { keys = [], keystrokes = 0, doors = 0 }


numberKey : Difficulty -> String
numberKey difficulty =
    String.fromInt (Difficulty.tarts difficulty)


{-| Press each key, then let Alice walk and read any story that pops up,
like a player who waits to see where they are before the next move.
-}
playRoute : Difficulty -> Run Hall.Model
playRoute difficulty =
    let
        send =
            Playthrough.send (Hall.update context)

        settle run =
            List.foldl
                (\_ current ->
                    if Story.isTalking current.model.story then
                        send (Hall.KeyPressed "Enter") current

                    else
                        send (Hall.Frame 16) current
                )
                run
                (List.range 1 400)

        pressKey key run =
            if String.all Char.isDigit key then
                send (Hall.KeyPressed key) run

            else
                settle (send (Hall.KeyPressed key) run)
    in
    Playthrough.start Hall.init
        |> Playthrough.sendAll (Hall.update context) (List.repeat 12 (Hall.KeyPressed "Enter"))
        |> send (Hall.KeyPressed (numberKey difficulty))
        |> (\run -> List.foldl pressKey run (List.concatMap splitCount (routeOf difficulty).keys))
        |> settle


{-| "12" is typed as "1" then "2".
-}
splitCount : String -> List String
splitCount key =
    if String.all Char.isDigit key then
        List.map String.fromChar (String.toList key)

    else
        [ key ]


directionsUsed : Route -> List String
directionsUsed route =
    route.keys |> List.filter (\key -> List.member key [ "h", "j", "k", "l" ])


turns : Route -> Int
turns route =
    let
        moves =
            directionsUsed route
    in
    List.map2 (/=) moves (List.drop 1 moves) |> List.filter identity |> List.length


{-| The places Alice can walk to from the start without using any door.
-}
withoutDoors : Map -> Set.Set ( Int, Int )
withoutDoors map =
    let
        open point =
            case Map.tileAt point map of
                Portal _ ->
                    False

                tile ->
                    Tiles.isWalkable tile

        spread todo seen =
            case todo of
                point :: rest ->
                    let
                        next =
                            Map.directions
                                |> List.map (\direction -> Map.neighbour direction point)
                                |> List.filter (\p -> open p && not (Set.member ( p.x, p.y ) seen))
                    in
                    spread (next ++ rest) (List.foldl (\p s -> Set.insert ( p.x, p.y ) s) seen next)

                [] ->
                    seen
    in
    case Map.find Start map of
        Just start ->
            spread [ start ] (Set.singleton ( start.x, start.y ))

        Nothing ->
            Set.empty


reachable : Tile -> Map -> Bool
reachable tile map =
    case Map.find tile map of
        Just { x, y } ->
            Set.member ( x, y ) (withoutDoors map)

        Nothing ->
            False


suite : Test
suite =
    describe "The halls of chapter 2"
        [ describe "every hall can be finished, and is worth its difficulty's tarts"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ ->
                            playRoute difficulty
                                |> (\run -> ( Playthrough.finishedWith run, run.model.keystrokes ))
                                |> Expect.equal ( Just (Difficulty.tarts difficulty), (routeOf difficulty).keystrokes )
                )
                Difficulty.all
            )
        , test "the easy hall is still today's hall: eight keystrokes at best" <|
            \_ -> (routeOf Easy).keystrokes |> Expect.equal 8
        , test "challenge: the best route uses all four directions" <|
            \_ ->
                directionsUsed (routeOf Challenge)
                    |> List.sort
                    |> List.foldr
                        (\key acc ->
                            if List.head acc == Just key then
                                acc

                            else
                                key :: acc
                        )
                        []
                    |> Expect.equal [ "h", "j", "k", "l" ]
        , test "challenge: the best route has at least 15 turns" <|
            \_ -> turns (routeOf Challenge) |> Expect.atLeast 15
        , test "mad: the key and the curtain can't be reached without doors" <|
            \_ ->
                ( reachable GoldenKey (mapOf Mad), reachable CurtainFront (mapOf Mad) )
                    |> Expect.equal ( False, False )
        , test "mad: the best route goes through at least three doors" <|
            \_ -> (routeOf Mad).doors |> Expect.atLeast 3
        , test "mad: every door has exactly one partner" <|
            \_ ->
                List.range 1 9
                    |> List.map (\number -> List.length (doorsNumbered number (mapOf Mad)))
                    |> List.filter (\count -> count /= 0 && count /= 2)
                    |> Expect.equal []
        , test "stepping into a door comes out of its partner, and stops there" <|
            \_ ->
                let
                    send =
                        Playthrough.send (Hall.update context)

                    walked =
                        Playthrough.start Hall.init
                            |> Playthrough.sendAll (Hall.update context) (List.repeat 12 (Hall.KeyPressed "Enter"))
                            |> send (Hall.KeyPressed "3")
                            -- From the start (1,10): up 4 to (1,6), right 2 to (3,6), down 2, right 2,
                            -- up 2 to (5,6), then right 3 reaches door 1 at (8,6); the extra steps of
                            -- "9l" must not carry on past the partner door.
                            |> Playthrough.sendAll (Hall.update context)
                                (List.map Hall.KeyPressed [ "4", "k", "2", "l", "2", "j", "2", "l", "2", "k", "9", "l" ])
                            |> Playthrough.sendAll (Hall.update context) (List.repeat 400 (Hall.Frame 16))
                in
                walked.model.alice
                    |> Expect.equal { x = 1, y = 1 }
        ]


doorsNumbered : Int -> Map -> List Point
doorsNumbered number map =
    List.range 0 11
        |> List.concatMap (\y -> List.range 0 19 |> List.map (\x -> { x = x, y = y }))
        |> List.filter (\point -> Map.tileAt point map == Portal number)


difficultyLabel : Difficulty -> String
difficultyLabel difficulty =
    Difficulty.name (Texts.forLanguage English) difficulty
