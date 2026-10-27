module Chapter.Hall.Map exposing
    ( Map, Point, plan, parse
    , tileAt, setTile, find, leadsTo
    , Direction(..), directions, neighbour, directionFor, keyFor
    )

{-| The halls of chapter 2: one map per difficulty, and how to look around
in them. Shared by the chapter, the route finder (`Chapter.Hall.Route`) and
the tests.

@docs Map, Point, plan, parse
@docs tileAt, setTile, find, leadsTo
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
-- A digit is a door that leads somewhere: the two doors with the same digit
-- lead to each other. The player never sees the digits; to them both look
-- like the hall's ordinary doors.
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
        -- hall's own doors connect them: the door on the left leads to the
        -- first door at the top, the middle top door to the door on the
        -- right, the right top door to the right bottom door.
        Mad ->
            [ "####1####2####3#####"
            , "#.....x.....x......#"
            , "#..PP...x...x.xPPx.#"
            , "#xxPPxxxTTTx...PP..#"
            , "#....x..TTT..x.....#"
            , "#....x...K..x...x..#"
            , "#....x..x..x.....x.#"
            , "1..x.PP......PP....2"
            , "#x...PPxxxxxxPPxxxx#"
            , "#..x...x...x....x..#"
            , "#S.....x..x...x...c#"
            , "#####D#######3####C#"
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


{-| Where Alice comes out after stepping into a door: on the floor in front
of its partner, the other door with the same number.
-}
leadsTo : Point -> Map -> Maybe Point
leadsTo door map =
    case tileAt door map of
        Portal number ->
            findAll (Portal number) map
                |> List.filter ((/=) door)
                |> List.head
                |> Maybe.andThen (\other -> inFrontOf other map)

        _ ->
            Nothing


{-| The floor next to a door in the outer wall.
-}
inFrontOf : Point -> Map -> Maybe Point
inFrontOf door map =
    directions
        |> List.map (\direction -> neighbour direction door)
        |> List.filter (\point -> Tiles.isWalkable (tileAt point map) && not (isDoor (tileAt point map)))
        |> List.head


isDoor : Tile -> Bool
isDoor tile =
    case tile of
        Portal _ ->
            True

        _ ->
            False



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
