module HallMapsTest exposing (suite)

{-| The three halls of chapter 2 keep the promises made for them:

  - every hall can be finished, by playing the route with the fewest
    keystrokes through the real chapter;
  - the harder halls are the easy hall with things dropped on the floor:
    walls, pillars, table, doors and key stay where they were;
  - challenge can be walked without counts; walking, it needs all four
    directions, many turns and no long straight runs, and always leaves
    more than one way open;
  - mad can't be finished without jumping over dropped things (#89): the
    way to the key can be walked, the rest needs counts of at most 5, in
    at least 10 moves.

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
    { texts = Texts.forLanguage English, layout = UsQwerty, escapeKeys = Nothing }


mapOf : Difficulty -> Map
mapOf difficulty =
    Map.parse (Map.plan difficulty)


startOf : Map -> Point
startOf map =
    Map.find Start map |> Maybe.withDefault { x = 1, y = 10 }


{-| The biggest count each hall accepts, as in the game.
-}
limitOf : Difficulty -> Int
limitOf difficulty =
    if difficulty == Mad then
        5

    else
        30


{-| The fewest keystrokes from the start.
-}
routeOf : Difficulty -> Route
routeOf difficulty =
    Route.fewest { largestCount = limitOf difficulty } { at = startOf (mapOf difficulty), hasKey = False } (mapOf difficulty)
        |> Maybe.withDefault { keys = [], keystrokes = 0, moves = 0 }


{-| The hall as if nothing could be jumped over: every dropped thing is a
wall.
-}
withoutJumps : Map -> Map
withoutJumps map =
    List.foldl
        (\point current ->
            if Map.tileAt point current == Dropped then
                Map.setTile point Wall current

            else
                current
        )
        map
        allPoints


{-| The fewest keystrokes walking only, counts allowed but no jumps.
-}
walkingRouteOf : Difficulty -> Maybe Route
walkingRouteOf difficulty =
    Route.fewest { largestCount = limitOf difficulty } { at = startOf (mapOf difficulty), hasKey = False } (withoutJumps (mapOf difficulty))


inHall : Difficulty -> Run Hall.Model
inHall difficulty =
    Playthrough.start Hall.init
        |> Playthrough.sendAll (Hall.update context) (List.repeat 12 (Hall.KeyPressed "Enter"))
        |> Playthrough.send (Hall.update context) (Hall.KeyPressed (numberKey difficulty))


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

        settle =
            settleWith send

        pressKey key run =
            if String.all Char.isDigit key then
                send (Hall.KeyPressed key) run

            else
                settle (send (Hall.KeyPressed key) run)
    in
    inHall difficulty
        |> (\run -> List.foldl pressKey run (List.concatMap splitCount (routeOf difficulty).keys))
        |> settle


{-| Let Alice walk and read any story that pops up.
-}
settleWith : (Hall.Msg -> Run Hall.Model -> Run Hall.Model) -> Run Hall.Model -> Run Hall.Model
settleWith send run =
    List.foldl
        (\_ current ->
            if Story.isTalking current.model.story then
                send (Hall.KeyPressed "Enter") current

            else
                send (Hall.Frame 16) current
        )
        run
        (List.range 1 400)


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


{-| The places Alice can walk to from the start without jumping.
-}
walkable : Map -> Set.Set ( Int, Int )
walkable map =
    let
        spread todo seen =
            case todo of
                point :: rest ->
                    let
                        next =
                            Map.directions
                                |> List.map (\direction -> Map.neighbour direction point)
                                |> List.filter (\p -> Tiles.isWalkable (Map.tileAt p map) && not (Set.member ( p.x, p.y ) seen))
                    in
                    spread (next ++ rest) (List.foldl (\p seenSoFar -> Set.insert ( p.x, p.y ) seenSoFar) seen next)

                [] ->
                    seen
    in
    spread [ startOf map ] (Set.singleton ( (startOf map).x, (startOf map).y ))


{-| Whether Alice can walk to `tile` from the start, without jumping. The
key lies on the table (#86): it counts as reached from any floor next to
it.
-}
reachable : Tile -> Map -> Bool
reachable tile map =
    case Map.find tile map of
        Just point ->
            (point :: List.map (\direction -> Map.neighbour direction point) Map.directions)
                |> List.any (\{ x, y } -> Set.member ( x, y ) (walkable map))

        Nothing ->
            False


suite : Test
suite =
    describe "The halls of chapter 2"
        [ describe "every hall can be finished in the fewest keystrokes, for its tarts"
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
        , test "the easy hall: eight keystrokes at best" <|
            \_ -> (routeOf Easy).keystrokes |> Expect.equal 8
        , test "challenge can be finished with single steps only, no counts (#89)" <|
            \_ ->
                Route.fewest { largestCount = 1 } { at = startOf (mapOf Challenge), hasKey = False } (mapOf Challenge)
                    |> Maybe.map (\route -> List.all (\key -> String.length key == 1 && not (String.all Char.isDigit key)) route.keys)
                    |> Expect.equal (Just True)
        , describe "challenge, walking without jumps"
            (let
                walking =
                    walkingRouteOf Challenge |> Maybe.withDefault { keys = [], keystrokes = 0, moves = 0 }
             in
             [ test "the best route uses all four directions" <|
                \_ ->
                    directionsUsed walking
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
             , test "the best route has at least 15 turns" <|
                \_ -> turns walking |> Expect.atLeast 15
             , test "no straight run is longer than 5 steps" <|
                \_ -> longestCount walking |> Expect.atMost 5
             , test "blocking any one tile still leaves a way through" <|
                \_ -> chokepoints (mapOf Challenge) |> Expect.equal []
             ]
            )
        , describe "the harder halls keep the easy hall's walls, pillars, table, doors and key"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ -> changedLayout (mapOf difficulty) |> Expect.equal []
                )
                [ Challenge, Mad ]
            )
        , describe "the key lies on the glass table, the bottle next to it (#86)"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ ->
                            ( Map.find GoldenKey (mapOf difficulty) |> Maybe.map (\key -> Map.tileAt { key | y = key.y - 1 } (mapOf difficulty))
                            , Map.find Bottle (mapOf difficulty) |> Maybe.map (\bottle -> Map.tileAt { bottle | x = bottle.x - 1 } (mapOf difficulty))
                            )
                                |> Expect.equal ( Just GlassTable, Just GoldenKey )
                )
                Difficulty.all
            )
        , describe "the way to the key can be walked in easy and challenge"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ -> reachable GoldenKey (mapOf difficulty) |> Expect.equal True
                )
                [ Easy, Challenge ]
            )
        , describe "mad: only counts get Alice to the curtain (#89)"
            [ test "walking alone, neither the key nor the curtain can be reached: the whole hall is full (#91)" <|
                \_ -> ( reachable GoldenKey (mapOf Mad), reachable CurtainFront (mapOf Mad) ) |> Expect.equal ( False, False )
            , test "with counts but no jumps, the hall can't be finished" <|
                \_ -> walkingRouteOf Mad |> Expect.equal Nothing
            , test "the fewest keystrokes are many more than in the other halls" <|
                \_ -> (routeOf Mad).keystrokes |> Expect.atLeast 30
            , test "even the fewest moves are at least 10" <|
                \_ ->
                    Route.fewestMoves { largestCount = 5 } { at = startOf (mapOf Mad), hasKey = False } (mapOf Mad)
                        |> Maybe.map .moves
                        |> Maybe.withDefault 0
                        |> Expect.atLeast 10
            , test "nearly every move of the best route uses a count" <|
                \_ ->
                    let
                        route =
                            routeOf Mad
                    in
                    toFloat (List.length (List.filter (String.all Char.isDigit) route.keys))
                        / toFloat route.moves
                        |> Expect.atLeast 0.8
            , test "no count above 5 is needed" <|
                \_ -> longestCount (routeOf Mad) |> Expect.atMost 5
            , test "with 5 as the most, 4 isn't enough: the limit matters" <|
                \_ ->
                    Route.fewest { largestCount = 4 } { at = startOf (mapOf Mad), hasKey = False } (mapOf Mad)
                        |> Maybe.map .keystrokes
                        |> Maybe.withDefault 999
                        |> Expect.greaterThan (routeOf Mad).keystrokes
            ]
        ]


allPoints : List Point
allPoints =
    List.range 0 11
        |> List.concatMap (\y -> List.range 0 19 |> List.map (\x -> { x = x, y = y }))


{-| Tiles that differ from the easy hall in a way other than something
dropped on its floor.
-}
changedLayout : Map -> List Point
changedLayout map =
    let
        easy =
            mapOf Easy

        allowed before after =
            case ( before, after ) of
                ( Floor, Dropped ) ->
                    True

                _ ->
                    before == after
    in
    allPoints |> List.filter (\point -> not (allowed (Map.tileAt point easy) (Map.tileAt point map)))


{-| Floor tiles that, blocked, would leave no way to finish the hall
walking, without jumps. Tiles
right next to the start, the key spot or the curtain don't count: those can
always be blocked off. The key lies on the table (#86), so the key spot is
the floor in front of it, where Alice takes it.
-}
chokepoints : Map -> List Point
chokepoints map =
    let
        nextTo tile point =
            List.any (\direction -> Map.tileAt (Map.neighbour direction point) map == tile) Map.directions

        nextToKeySpot point =
            nextTo GoldenKey point
                || List.any
                    (\direction ->
                        let
                            spot =
                                Map.neighbour direction point
                        in
                        Map.tileAt spot map == Floor && nextTo GoldenKey spot
                    )
                    Map.directions
    in
    allPoints
        |> List.filter (\point -> Map.tileAt point map == Floor)
        |> List.filter (\point -> not (nextTo Start point || nextToKeySpot point || nextTo CurtainFront point))
        |> List.filter (\point -> Route.fewest { largestCount = 30 } { at = startOf map, hasKey = False } (withoutJumps (Map.setTile point Wall map)) == Nothing)


longestCount : Route -> Int
longestCount route =
    route.keys
        |> List.filterMap String.toInt
        |> List.maximum
        |> Maybe.withDefault 1


difficultyLabel : Difficulty -> String
difficultyLabel difficulty =
    Difficulty.name (Texts.forLanguage English) difficulty
