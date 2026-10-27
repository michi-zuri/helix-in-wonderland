module Chapter.Hall.Route exposing (Route, fewest)

{-| The route through a hall with the fewest keystrokes, counts included.

The game shows this number on the results card ("fewest possible"), and the
tests play the route to prove every hall can be finished. In the mad hall
the doors change where they lead, so there it is the fewest keystrokes with
the luckiest doors.

A move is one count and a direction: `7l` walks seven steps right for two
keystrokes, `l` one step for one. A move stops early, as in the game: at a
wall, at the key (picking it up), at the curtain, and at a door (which
leads on to the floor in front of another door).

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


{-| The fewest keystrokes from `from` to the curtain with the key.

`largestCount` is the biggest count the game accepts. `exits` says which
doors a door might lead to; with several, the route takes the luckiest.

-}
fewest :
    { largestCount : Int, exits : Point -> List Point }
    -> { at : Point, hasKey : Bool }
    -> Map
    -> Maybe Route
fewest { largestCount, exits } from map =
    search { largestCount = largestCount, exits = exits, map = map }
        Set.empty
        [ { cost = 0, at = from.at, hasKey = from.hasKey, won = False, keys = [], doors = 0 } ]


type alias Hall =
    { largestCount : Int
    , exits : Point -> List Point
    , map : Map
    }


{-| Dijkstra's algorithm: always continue from the cheapest position not
yet explored, so the first time the curtain is reached, it was reached in
the fewest keystrokes.
-}
search : Hall -> Set ( Int, Int, Int ) -> List Visit -> Maybe Route
search hall explored frontier =
    case cheapest frontier of
        Nothing ->
            Nothing

        Just ( visit, rest ) ->
            if visit.won then
                Just { keys = List.reverse visit.keys, keystrokes = visit.cost, doors = visit.doors }

            else if Set.member (place visit) explored then
                search hall explored rest

            else
                search hall
                    (Set.insert (place visit) explored)
                    (rest ++ List.concatMap (moves hall visit) Map.directions)


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
moves : Hall -> Visit -> Direction -> List Visit
moves { largestCount, exits, map } visit direction =
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

                    Portal ->
                        exits next
                            |> List.filterMap (\door -> Map.inFrontOf door map)
                            |> List.map (\exit -> arriving (\v -> { v | at = exit, doors = v.doors + 1 }))
                            |> (\arrivals -> arrivals ++ found)

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
