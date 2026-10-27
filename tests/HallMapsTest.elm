module HallMapsTest exposing (suite)

{-| The three halls of chapter 2 keep the promises made for them:

  - every hall can be finished, by playing the route with the fewest
    keystrokes through the real chapter;
  - the harder halls are the easy hall with things dropped on the floor:
    walls, pillars, table, doors and key stay where they were;
  - challenge needs all four directions, many turns and no long straight
    runs, and always leaves more than one way open;
  - in mad, parts of the hall only the doors connect; however the doors
    shuffle, a player who keeps trying gets through.

-}

import Chapter.Contract exposing (Context)
import Chapter.Hall as Hall
import Chapter.Hall.Doors as Doors
import Chapter.Hall.Map as Map exposing (Map, Point)
import Chapter.Hall.Route as Route exposing (Route)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Chapter.Story as Story
import Difficulty exposing (Difficulty(..))
import Expect
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough exposing (Run)
import Random
import Set
import Test exposing (Test, describe, test)
import Texts


context : Context
context =
    { texts = Texts.forLanguage English, layout = UsQwerty }


mapOf : Difficulty -> Map
mapOf difficulty =
    Map.parse (Map.plan difficulty)


{-| The fewest keystrokes from the start; in the mad hall with the luckiest
doors.
-}
routeOf : Difficulty -> Route
routeOf difficulty =
    let
        map =
            mapOf difficulty

        doors =
            Doors.init (Random.initialSeed 1) map
    in
    Route.fewest { largestCount = 30, exits = \door -> Doors.mightLeadTo door doors }
        { at = Map.find Start map |> Maybe.withDefault { x = 1, y = 10 }, hasKey = False }
        map
        |> Maybe.withDefault { keys = [], keystrokes = 0, doors = 0 }


inHall : Int -> Difficulty -> Run Hall.Model
inHall seed difficulty =
    Playthrough.start (Hall.init (Random.initialSeed seed))
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
    inHall 1 difficulty
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


{-| A player in the mad hall: plan the best route with the luckiest doors
from wherever Alice is, make its first move, look where she ended up, plan
again. Gives up after `movesLeft` moves.
-}
playMad : Int -> Run Hall.Model -> Run Hall.Model
playMad movesLeft run =
    let
        send =
            Playthrough.send (Hall.update context)

        model =
            run.model

        plan =
            Route.fewest { largestCount = 30, exits = \door -> Doors.mightLeadTo door model.doors }
                { at = model.alice, hasKey = model.hasKey }
                model.map
    in
    if movesLeft <= 0 || Playthrough.finishedWith run /= Nothing then
        run

    else
        case plan |> Maybe.map (.keys >> firstMove) of
            Just ((_ :: _) as move) ->
                playMad (movesLeft - 1)
                    (settleWith send (List.foldl (\key current -> send (Hall.KeyPressed key) current) run move))

            _ ->
                run


{-| The keys of the first move: an optional count, then a direction.
-}
firstMove : List String -> List String
firstMove keys =
    case keys of
        first :: rest ->
            if String.all Char.isDigit first then
                splitCount first ++ List.take 1 rest

            else
                [ first ]

        [] ->
            []


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
                Portal ->
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
        [ describe "easy and challenge can be finished in the fewest keystrokes, for their tarts"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ ->
                            playRoute difficulty
                                |> (\run -> ( Playthrough.finishedWith run, run.model.keystrokes ))
                                |> Expect.equal ( Just (Difficulty.tarts difficulty), (routeOf difficulty).keystrokes )
                )
                [ Easy, Challenge ]
            )
        , describe "mad: however the doors shuffle, a player who keeps trying earns three tarts"
            (List.map
                (\seed ->
                    test ("seed " ++ String.fromInt seed) <|
                        \_ ->
                            inHall seed Mad
                                |> playMad 200
                                |> (\run -> ( Playthrough.finishedWith run, run.model.keystrokes >= (routeOf Mad).keystrokes ))
                                |> Expect.equal ( Just 3, True )
                )
                (List.range 1 20)
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
        , test "challenge: no straight run is longer than 5 steps" <|
            \_ -> longestCount (routeOf Challenge) |> Expect.atMost 5
        , test "challenge: blocking any one tile still leaves a way through" <|
            \_ ->
                chokepoints (mapOf Challenge) |> Expect.equal []
        , describe "the harder halls keep the easy hall's walls, pillars, table, doors and key"
            (List.map
                (\difficulty ->
                    test (difficultyLabel difficulty) <|
                        \_ -> changedLayout (mapOf difficulty) |> Expect.equal []
                )
                [ Challenge, Mad ]
            )
        , test "mad: the key and the curtain can't be reached without doors" <|
            \_ ->
                ( reachable GoldenKey (mapOf Mad), reachable CurtainFront (mapOf Mad) )
                    |> Expect.equal ( False, False )
        , test "mad: even with the luckiest doors, the way needs at least two of them" <|
            \_ -> (routeOf Mad).doors |> Expect.atLeast 2
        , test "stepping into a door comes out in front of the door it leads to, and stops there" <|
            \_ ->
                let
                    leftDoor =
                        { x = 0, y = 7 }

                    expected =
                        Doors.leadsTo leftDoor (Doors.init (Random.initialSeed 7) (mapOf Mad))
                            |> Maybe.andThen (\door -> Map.inFrontOf door (mapOf Mad))

                    walked =
                        inHall 7 Mad
                            -- From the start (1,10): right to (2,10), up 3 to (2,7), then left:
                            -- the second step enters the door on the left; the extra steps of
                            -- "9h" must not carry on after Alice comes out.
                            |> Playthrough.sendAll (Hall.update context)
                                (List.map Hall.KeyPressed [ "l", "3", "k", "9", "h" ])
                            |> Playthrough.sendAll (Hall.update context) (List.repeat 400 (Hall.Frame 16))
                in
                Just walked.model.alice
                    |> Expect.equal expected
        ]


allPoints : List Point
allPoints =
    List.range 0 11
        |> List.concatMap (\y -> List.range 0 19 |> List.map (\x -> { x = x, y = y }))


{-| Tiles that differ from the easy hall in a way other than something
dropped on its floor or one of its doors leading somewhere.
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

                ( Door, Portal ) ->
                    True

                _ ->
                    before == after
    in
    allPoints |> List.filter (\point -> not (allowed (Map.tileAt point easy) (Map.tileAt point map)))


{-| Floor tiles that, blocked, would leave no way to finish the hall. Tiles
right next to the start, the key or the curtain don't count: those can
always be blocked off.
-}
chokepoints : Map -> List Point
chokepoints map =
    let
        nextTo tile point =
            List.any (\direction -> Map.tileAt (Map.neighbour direction point) map == tile) Map.directions
    in
    allPoints
        |> List.filter (\point -> Map.tileAt point map == Floor)
        |> List.filter (\point -> not (nextTo Start point || nextTo GoldenKey point || nextTo CurtainFront point))
        |> List.filter (\point -> Route.fewest { largestCount = 30, exits = always [] } { at = Map.find Start map |> Maybe.withDefault point, hasKey = False } (Map.setTile point Dropped map) == Nothing)


longestCount : Route -> Int
longestCount route =
    route.keys
        |> List.filterMap String.toInt
        |> List.maximum
        |> Maybe.withDefault 1


difficultyLabel : Difficulty -> String
difficultyLabel difficulty =
    Difficulty.name (Texts.forLanguage English) difficulty
