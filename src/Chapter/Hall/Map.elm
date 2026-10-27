module Chapter.Hall.Map exposing
    ( Map, Point, plan, parse
    , tileAt, setTile, find, partner
    , Direction(..), directions, neighbour, directionFor, keyFor
    )

{-| The halls of chapter 2: one map per difficulty, and how to look around
in them. Shared by the chapter, the route finder (`Chapter.Hall.Route`) and
the tests.

@docs Map, Point, plan, parse
@docs tileAt, setTile, find, partner
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
-- goal), T glass table, P pillar, K golden key, S start, . floor.
-- A digit is a door that leads somewhere: the two doors with the same digit
-- lead to each other.


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

        -- One winding corridor: back and forth up the left half (the key
        -- waits in a notch at the top), then up and down the right half.
        -- Turns in all four directions, about 20 of them.
        Challenge ->
            [ "####D####D####D#####"
            , "#####K######...#...#"
            , "#..........#...#...#"
            , "#.########.#.#.#.#.#"
            , "#........#.#.#.#.#.#"
            , "########.#.#.#.#.#.#"
            , "#........#.#.#.#.#.#"
            , "#.########.#.#.#.#.#"
            , "#........#.#.#.#.#.#"
            , "########.#...#...#.#"
            , "#S.......#...#...#c#"
            , "#####D#######D####C#"
            ]

        -- Four rooms sealed off from each other, each a small maze. Only
        -- the doors connect them: start room, door 1, door 2, the key's
        -- room, door 3, the curtain's room.
        Mad ->
            [ "####D####D####D#####"
            , "#1..#...2##3.#...#K#"
            , "#.#.#.######.#.#.#.#"
            , "#.#...#..##....#...#"
            , "#...###..##2########"
            , "####################"
            , "#...#...1##3..#....#"
            , "#.#.#.#######.#.##.#"
            , "#.#...#..##.....#..#"
            , "#.#####.#########.##"
            , "#S......###.......c#"
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


{-| Where a door leads: the other door with the same number.
-}
partner : Point -> Map -> Maybe Point
partner door map =
    case tileAt door map of
        Portal number ->
            findAll (Portal number) map
                |> List.filter ((/=) door)
                |> List.head

        _ ->
            Nothing



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
