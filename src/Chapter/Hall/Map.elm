module Chapter.Hall.Map exposing
    ( Map, Point, plan, parse
    , tileAt, setTile, find, findAll, inFrontOf
    , Direction(..), directions, neighbour, directionFor, keyFor
    )

{-| The halls of chapter 2: one map per difficulty, and how to look around
in them. Shared by the chapter, the route finder (`Chapter.Hall.Route`) and
the tests.

@docs Map, Point, plan, parse
@docs tileAt, setTile, find, findAll, inFrontOf
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
-- goal), T glass table, P pillar, K golden key, S start, . floor,
-- x something dropped on the floor (blocks the way).
-- O is a door that leads somewhere: to another O door, which one is decided
-- while playing (see `Chapter.Hall.Doors`). To the player it looks like any
-- other door.
--
-- The harder halls are the easy hall with things dropped on its floor and
-- some doors opened: walls, pillars, table, doors and key stay where they
-- are (a test checks this).


plan : Difficulty -> List String
plan difficulty =
    case difficulty of
        -- The open hall. Fewest keystrokes: 5k 8l 9l 5j.
        Easy ->
            [ "####D####D####D#####"
            , "#..................#"
            , "#..PP..........PP..#"
            , "#..PP...TTT....PP..#"
            , "#.......TTT........#"
            , "#........K.........#"
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
            , "#xx...x.TTT......x.#"
            , "#...x...xK...x.....#"
            , "#.x...x..x.....x.x.#"
            , "D..x.PP.x.x..PP.x..D"
            , "#.x..PP...xx.PP...x#"
            , "#...x...x...x...x..#"
            , "#S..x..x......x...c#"
            , "#####D#######D####C#"
            ]

        -- Heaps of dropped things split the hall into four parts. Only the
        -- hall's own doors connect them, and where each door leads changes
        -- as Alice uses them (`Chapter.Hall.Doors`).
        Mad ->
            [ "####O####O####O#####"
            , "#.....x.....x......#"
            , "#..PP...x...x.xPPx.#"
            , "#xxPPxxxTTTx...PP..#"
            , "#....x..TTT..x.....#"
            , "#....x...K..x...x..#"
            , "#....x..x..x.....x.#"
            , "O..x.PP......PP....O"
            , "#x...PPxxxxxxPPxxxx#"
            , "#..x...x...x....x..#"
            , "#S.....x..x...x...c#"
            , "#####D#######O####C#"
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


{-| The floor next to a door in the outer wall: where Alice comes out.
-}
inFrontOf : Point -> Map -> Maybe Point
inFrontOf door map =
    directions
        |> List.map (\direction -> neighbour direction door)
        |> List.filter (\point -> Tiles.isWalkable (tileAt point map) && tileAt point map /= Portal)
        |> List.head



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
