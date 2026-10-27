module Chapter.Hall.Route exposing (Route, fewest)

{-| The route through a hall with the fewest keystrokes, counts included.

The game shows this number on the results card ("fewest possible"), and the
tests play the route to prove every hall can be finished.

A move is one count and a direction: `7l` walks seven steps right for two
keystrokes, `l` one step for one. A move stops early, as in the game: at a
wall, at the key (picking it up), at the curtain, and at a door (which
leads to its partner door).

-}

import Chapter.Hall.Map as Map exposing (Direction, Map, Point)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Set exposing (Set)


type alias Route =
    { keys : List String
    , keystrokes : Int
    , doors : Int
    }


{-| A position on the way: where Alice stands and whether she has the key.
`keys` and `doors` record how she got there (keys newest first).
-}
type alias Visit =
    { cost : Int
    , at : Point
    , hasKey : Bool
    , won : Bool
    , keys : List String
    , doors : Int
    }


{-| `largestCount` is the biggest count the game accepts.
-}
fewest : Int -> Map -> Maybe Route
fewest largestCount map =
    Map.find Start map
        |> Maybe.andThen
            (\start ->
                search largestCount map Set.empty [ { cost = 0, at = start, hasKey = False, won = False, keys = [], doors = 0 } ]
            )


{-| Dijkstra's algorithm: always continue from the cheapest position not
yet explored, so the first time the curtain is reached, it was reached in
the fewest keystrokes.
-}
search : Int -> Map -> Set ( Int, Int, Int ) -> List Visit -> Maybe Route
search largestCount map explored frontier =
    case cheapest frontier of
        Nothing ->
            Nothing

        Just ( visit, rest ) ->
            if visit.won then
                Just { keys = List.reverse visit.keys, keystrokes = visit.cost, doors = visit.doors }

            else if Set.member (place visit) explored then
                search largestCount map explored rest

            else
                search largestCount
                    map
                    (Set.insert (place visit) explored)
                    (rest ++ List.concatMap (moves largestCount map visit) Map.directions)


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


{-| Every move from `visit` in one direction: one step, two steps, ... until
something stops Alice.
-}
moves : Int -> Map -> Visit -> Direction -> List Visit
moves largestCount map visit direction =
    let
        go steps at found =
            let
                next =
                    Map.neighbour direction at

                tile =
                    Map.tileAt next map

                arriving extra =
                    { visit
                        | cost = visit.cost + keystrokesFor steps
                        , at = next
                        , keys = keysFor steps direction ++ visit.keys
                    }
                        |> extra
            in
            if steps > largestCount || not (Tiles.isWalkable tile) then
                found

            else
                case tile of
                    GoldenKey ->
                        if visit.hasKey then
                            go (steps + 1) next (arriving identity :: found)

                        else
                            arriving (\v -> { v | hasKey = True }) :: found

                    Portal _ ->
                        case Map.partner next map of
                            Just other ->
                                arriving (\v -> { v | at = other, doors = v.doors + 1 }) :: found

                            Nothing ->
                                found

                    CurtainFront ->
                        if visit.hasKey then
                            arriving (\v -> { v | won = True }) :: found

                        else
                            arriving identity :: found

                    _ ->
                        go (steps + 1) next (arriving identity :: found)
    in
    go 1 visit.at []


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
