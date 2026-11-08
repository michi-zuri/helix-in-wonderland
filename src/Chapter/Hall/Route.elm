module Chapter.Hall.Route exposing (Route, fewest, fewestMoves)

{-| The route through a hall with the fewest keystrokes, counts included.

The game shows this number on the results card ("fewest possible"), and the
tests play the route to prove every hall can be finished.

A move is one count and a direction: `7l` walks seven steps right for two
keystrokes, `l` one step for one. What a move does (stopping at walls,
taking the key, jumping over dropped things) is `Chapter.Hall.Move`, the
same rules the game plays by.

-}

import Chapter.Hall.Map as Map exposing (Direction, Map, Point)
import Chapter.Hall.Move as Move exposing (Ending(..))
import Chapter.Hall.Tiles exposing (Tile(..))
import Set exposing (Set)


{-| `keys` to press, how many `keystrokes` that is, and in how many `moves`
(a count and its direction are one move).
-}
type alias Route =
    { keys : List String
    , keystrokes : Int
    , moves : Int
    }


{-| A position on the way: where Alice stands and whether she has the key.
`keys` records how she got there (newest first).
-}
type alias Visit =
    { cost : Int
    , at : Point
    , hasKey : Bool
    , won : Bool
    , keys : List String
    , keystrokes : Int
    , moves : Int
    }


{-| The fewest keystrokes from `from` to the curtain with the key.
`largestCount` is the biggest count the hall accepts.
-}
fewest : { largestCount : Int } -> { at : Point, hasKey : Bool } -> Map -> Maybe Route
fewest { largestCount } =
    start { largestCount = largestCount, costOf = keystrokesFor }


{-| The fewest moves, however many keystrokes they take: how many times the
player has to decide on a direction.
-}
fewestMoves : { largestCount : Int } -> { at : Point, hasKey : Bool } -> Map -> Maybe Route
fewestMoves { largestCount } =
    start { largestCount = largestCount, costOf = always 1 }


type alias Rules =
    { largestCount : Int
    , costOf : Int -> Int
    }


start : Rules -> { at : Point, hasKey : Bool } -> Map -> Maybe Route
start rules from map =
    search rules
        map
        Set.empty
        [ { cost = 0, at = from.at, hasKey = from.hasKey, won = False, keys = [], keystrokes = 0, moves = 0 } ]


{-| Dijkstra's algorithm: always continue from the cheapest position not
yet explored, so the first time the curtain is reached, it was reached at
the lowest cost.
-}
search : Rules -> Map -> Set ( Int, Int, Int ) -> List Visit -> Maybe Route
search rules map explored frontier =
    case cheapest frontier of
        Nothing ->
            Nothing

        Just ( visit, rest ) ->
            if visit.won then
                Just { keys = List.reverse visit.keys, keystrokes = visit.keystrokes, moves = visit.moves }

            else if Set.member (place visit) explored then
                search rules map explored rest

            else
                search rules
                    map
                    (Set.insert (place visit) explored)
                    (rest ++ List.concatMap (moves rules map visit) Map.directions)


{-| Where a visit stands, as something a `Set` can hold (Bools can't be
compared, so the key becomes 0 or 1).
-}
place : Visit -> ( Int, Int, Int )
place visit =
    ( visit.at.x
    , visit.at.y
    , if visit.hasKey then
        1

      else
        0
    )


cheapest : List Visit -> Maybe ( Visit, List Visit )
cheapest visits =
    case visits of
        first :: others ->
            let
                best =
                    List.foldl
                        (\visit current ->
                            if visit.cost < current.cost then
                                visit

                            else
                                current
                        )
                        first
                        others
            in
            Just ( best, removeFirst best visits )

        [] ->
            Nothing


removeFirst : a -> List a -> List a
removeFirst item list =
    case list of
        first :: rest ->
            if first == item then
                rest

            else
                first :: removeFirst item rest

        [] ->
            []


{-| Every move from `visit` in one direction, for every count up to the
largest, that gets Alice somewhere new or takes the key. Once the key is
taken, its tile is plain glass table, which a `GoldenKey` still on the map
stands for here.
-}
moves : Rules -> Map -> Visit -> Direction -> List Visit
moves rules map visit direction =
    List.range 1 rules.largestCount
        |> List.filterMap
            (\count ->
                Move.resolve direction count visit.at map
                    |> Maybe.andThen
                        (\move ->
                            let
                                end =
                                    List.reverse move.path |> List.head |> Maybe.withDefault visit.at

                                takesKey =
                                    case move.ending of
                                        TakesKey _ ->
                                            not visit.hasKey

                                        _ ->
                                            False
                            in
                            if end == visit.at && not takesKey then
                                Nothing

                            else
                                Just
                                    { cost = visit.cost + rules.costOf count
                                    , at = end
                                    , hasKey = visit.hasKey || takesKey
                                    , won = (visit.hasKey || takesKey) && Map.tileAt end map == CurtainFront
                                    , keys = keysFor count direction ++ visit.keys
                                    , keystrokes = visit.keystrokes + keystrokesFor count
                                    , moves = visit.moves + 1
                                    }
                        )
            )


{-| One step is one key; more steps need a count first: `7l` is two keys.
-}
keystrokesFor : Int -> Int
keystrokesFor steps =
    if steps == 1 then
        1

    else
        1 + String.length (String.fromInt steps)


{-| The keys newest first, matching how a visit stores them.
-}
keysFor : Int -> Direction -> List String
keysFor steps direction =
    if steps == 1 then
        [ Map.keyFor direction ]

    else
        [ Map.keyFor direction, String.fromInt steps ]
