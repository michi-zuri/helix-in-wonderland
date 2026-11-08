module Chapter.Hall.Map exposing
    ( Map, Point, plan, parse
    , tileAt, setTile, find
    , Direction(..), directions, neighbour, directionFor, keyFor
    )

{-| The halls of chapter 2: one map per difficulty, and how to look around
in them. Shared by the chapter, the route finder (`Chapter.Hall.Route`) and
the tests.

@docs Map, Point, plan, parse
@docs tileAt, setTile, find
@docs Direction, directions, neighbour, directionFor, keyFor

-}

import Array exposing (Array)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Difficulty exposing (Difficulty(..))


type alias Map =
    Array (Array Tile)


type alias Point =
    { x : Int, y : Int }



-- THE HALLS
--
-- Legend: # wall, D door in the wall, C curtain (with c in front of it, the
-- goal), T glass table, K the golden key lying on it, B the bottle standing
-- next to it, P pillar, S start, . floor,
-- x something dropped on the floor: a single step bumps into it, a count
-- jumps over it (`Chapter.Hall.Move`). Every door is locked.
--
-- The harder halls are the easy hall with things dropped on its floor:
-- walls, pillars, table, doors and key stay where they are (a test checks
-- this).


plan : Difficulty -> List String
plan difficulty =
    case difficulty of
        -- The open hall. Fewest keystrokes: 8l 6k 5j 9l (a count that runs
        -- into the key on the table takes it).
        Easy ->
            [ "####D####D####D#####"
            , "#..................#"
            , "#..PP..........PP..#"
            , "#..PP...TTT....PP..#"
            , "#.......TKB........#"
            , "#..................#"
            , "#..................#"
            , "D....PP......PP....D"
            , "#....PP......PP....#"
            , "#..................#"
            , "#S................c#"
            , "#####D#######D####C#"
            ]

        -- The same hall with things dropped all over the floor. Every long
        -- straight line is broken, so the way twists in all four directions,
        -- but there is always more than one way round.
        Challenge ->
            [ "####D####D####D#####"
            , "#...x.....x....x...#"
            , "#.xPP.x.x...x..PPx.#"
            , "#..PP..xTTTx...PP.x#"
            , "#xx...x.TKB......x.#"
            , "#...x...x....x.....#"
            , "#.x...x..x.....x.x.#"
            , "D..x.PP.x.x..PP.x..D"
            , "#.x..PP...xx.PP...x#"
            , "#...x...x...x...x..#"
            , "#S..x..x......x...c#"
            , "#####D#######D####C#"
            ]

        -- The way to the key can be walked. Behind it, heaps of dropped
        -- things fill the hall, with a few free stepping stones: only counts
        -- get Alice across (#89), and 5 is the most here. Fewest moves: 12;
        -- fewest keystrokes: 3l 5k 5l k 2h 4k l 5l 5l 2j 4j 3j (22).
        Mad ->
            [ "####D####D####D#####"
            , "#.........xxx.xxx..#"
            , "#..PP.....xxxxxPPxx#"
            , "#..PP...TTTxxxxPPx.#"
            , "#.......TKBxxxxx.xx#"
            , "#.........xx.xxxxxx#"
            , "#.........xxxxxxxxx#"
            , "D....PP...xxxPPxxx.D"
            , "#....PP...xxxPPxxxx#"
            , "#.........xx.xxxxx.#"
            , "#S........xxxxxxxxc#"
            , "#####D#######D####C#"
            ]


parse : List String -> Map
parse rows =
    rows
        |> List.map (String.toList >> List.map Tiles.fromChar >> Array.fromList)
        |> Array.fromList



-- LOOKING AROUND


tileAt : Point -> Map -> Tile
tileAt { x, y } map =
    Array.get y map
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault Wall


setTile : Point -> Tile -> Map -> Map
setTile { x, y } tile map =
    case Array.get y map of
        Just row ->
            Array.set y (Array.set x tile row) map

        Nothing ->
            map


findAll : Tile -> Map -> List Point
findAll wanted map =
    map
        |> Array.toIndexedList
        |> List.concatMap
            (\( y, row ) ->
                row
                    |> Array.toIndexedList
                    |> List.filter (\( _, tile ) -> tile == wanted)
                    |> List.map (\( x, _ ) -> { x = x, y = y })
            )


find : Tile -> Map -> Maybe Point
find wanted map =
    findAll wanted map |> List.head



-- DIRECTIONS


type Direction
    = Left
    | Down
    | Up
    | Right


directions : List Direction
directions =
    [ Left, Down, Up, Right ]


neighbour : Direction -> Point -> Point
neighbour direction { x, y } =
    case direction of
        Left ->
            { x = x - 1, y = y }

        Right ->
            { x = x + 1, y = y }

        Up ->
            { x = x, y = y - 1 }

        Down ->
            { x = x, y = y + 1 }


directionFor : String -> Maybe Direction
directionFor key =
    case key of
        "h" ->
            Just Left

        "j" ->
            Just Down

        "k" ->
            Just Up

        "l" ->
            Just Right

        _ ->
            Nothing


keyFor : Direction -> String
keyFor direction =
    case direction of
        Left ->
            "h"

        Down ->
            "j"

        Up ->
            "k"

        Right ->
            "l"
