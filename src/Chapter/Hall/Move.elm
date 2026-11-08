module Chapter.Hall.Move exposing (Ending(..), Move, resolve)

{-| What one move does in the hall: a direction, with a count or without.
Shared by the chapter and the route finder (`Chapter.Hall.Route`), so the
fewest keystrokes the results card shows follow exactly the game's rules.

  - Alice walks up to `count` tiles in the direction.
  - Things dropped on the floor are jumped over (#89): a count carries her
    across them. A single step into one is a bump.
  - A wall, pillar, the table, the bottle or the curtain stop her early:
    she bumps into it. A door stops her too: she knocks, and it is locked.
  - Walking into the golden key on the table takes it (#86); she stays in
    front of the table.
  - The floor in front of the curtain stops a count: that is the goal.
  - A move that would end on something dropped fails as a whole: Alice
    bumps and stays where she is.

-}

import Chapter.Hall.Map as Map exposing (Direction, Map, Point)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))


{-| `path` is every tile Alice moves onto, in order; she ends on the last
one (an empty path: she doesn't move). `ending` says what happens there.
-}
type alias Move =
    { path : List Point
    , ending : Ending
    }


type Ending
    = Stops
    | TakesKey Point
    | KnocksAt
    | Bumps


{-| `Nothing` when the move fails as a whole: it would end on something
dropped on the floor.
-}
resolve : Direction -> Int -> Point -> Map -> Maybe Move
resolve direction count from map =
    let
        finish path ending =
            case List.head path of
                Just last ->
                    if Map.tileAt last map == Dropped then
                        Nothing

                    else
                        Just { path = List.reverse path, ending = ending }

                Nothing ->
                    Just { path = [], ending = ending }

        -- `path` is newest first while going.
        go steps at path =
            if steps > count then
                finish path Stops

            else
                let
                    next =
                        Map.neighbour direction at

                    tile =
                        Map.tileAt next map
                in
                case tile of
                    Dropped ->
                        go (steps + 1) next (next :: path)

                    CurtainFront ->
                        finish (next :: path) Stops

                    GoldenKey ->
                        finish path (TakesKey next)

                    Door ->
                        finish path KnocksAt

                    _ ->
                        if Tiles.isWalkable tile then
                            go (steps + 1) next (next :: path)

                        else
                            finish path Bumps
    in
    go 1 from []
