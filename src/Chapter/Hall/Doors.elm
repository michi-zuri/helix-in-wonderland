module Chapter.Hall.Doors exposing
    ( Doors, init
    , leadsTo, mightLeadTo, walkThrough
    , pairs
    )

{-| Where the doors of a hall lead. Mad mode's doors play tricks:

  - every door leads to another door, picked at random;
  - the way back always works: step back into the door you came out of
    and you return to the door you came from;
  - so after each trip only the pair just used stays linked, and all the
    other doors get new partners.

Two kinds of pairs could trap Alice, so they never happen: two doors of the
same part of the hall (she could never leave that part), and the two doors
that are each the only door of their part (she would go back and forth
between two dead ends forever).

The randomness comes from a seed, so the same seed plays the same game.

@docs Doors, init
@docs leadsTo, mightLeadTo, walkThrough
@docs pairs

-}

import Chapter.Hall.Map as Map exposing (Map, Point)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Dict exposing (Dict)
import Random
import Set exposing (Set)


type Doors
    = Doors
        { pairs : List ( Point, Point )
        , kept : Maybe ( Point, Point )
        , partOf : Dict ( Int, Int ) ( Int, Int )
        , deadEnds : Set ( Int, Int )
        , seed : Random.Seed
        }


{-| Find the hall's doors that lead somewhere and link them at random.
-}
init : Random.Seed -> Map -> Doors
init seed map =
    let
        doors =
            Map.findAll Portal map

        partOf =
            doors
                |> List.map (\door -> ( key door, part map door ))
                |> Dict.fromList

        doorsPerPart =
            Dict.foldl (\_ p counts -> Dict.update p (\n -> Just (Maybe.withDefault 0 n + 1)) counts) Dict.empty partOf

        deadEnds =
            Dict.toList partOf
                |> List.filter (\( _, p ) -> Dict.get p doorsPerPart == Just 1)
                |> List.map Tuple.first
                |> Set.fromList

        empty =
            { pairs = [], kept = Nothing, partOf = partOf, deadEnds = deadEnds, seed = seed }

        ( first, nextSeed ) =
            pickPairs empty doors
    in
    Doors { empty | pairs = first, seed = nextSeed }


{-| The door that `door` leads to right now.
-}
leadsTo : Point -> Doors -> Maybe Point
leadsTo door (Doors doors) =
    doors.pairs
        |> List.filterMap
            (\( a, b ) ->
                if a == door then
                    Just b

                else if b == door then
                    Just a

                else
                    Nothing
            )
        |> List.head


{-| Where `door` might lead, as far as the player can know: back to where
they came from if it's the way back, otherwise any door it may be linked
to. The route finder uses this for the fewest keystrokes possible with the
luckiest doors.
-}
mightLeadTo : Point -> Doors -> List Point
mightLeadTo door (Doors doors) =
    case doors.kept of
        Just ( a, b ) ->
            if door == a then
                [ b ]

            else if door == b then
                [ a ]

            else
                anyAllowed door doors

        Nothing ->
            anyAllowed door doors


anyAllowed : Point -> { a | partOf : Dict ( Int, Int ) ( Int, Int ), deadEnds : Set ( Int, Int ) } -> List Point
anyAllowed door doors =
    Dict.keys doors.partOf
        |> List.map (\( x, y ) -> { x = x, y = y })
        |> List.filter (\other -> other /= door && allowed doors ( door, other ))


{-| Alice went through `door`: that pair stays linked, every other door
gets a new partner.
-}
walkThrough : Point -> Doors -> Doors
walkThrough door (Doors doors) =
    case leadsTo door (Doors doors) of
        Just partner ->
            let
                others =
                    Dict.keys doors.partOf
                        |> List.map (\( x, y ) -> { x = x, y = y })
                        |> List.filter (\other -> other /= door && other /= partner)

                ( newPairs, nextSeed ) =
                    pickPairs doors others
            in
            Doors { doors | pairs = ( door, partner ) :: newPairs, kept = Just ( door, partner ), seed = nextSeed }

        Nothing ->
            Doors doors


{-| All current pairs, for tests.
-}
pairs : Doors -> List ( Point, Point )
pairs (Doors doors) =
    doors.pairs



-- PAIRING


{-| Pick one of the allowed ways to pair up `doors`, at random. There are
few doors, so it simply lists every way and picks one.
-}
pickPairs :
    { a | partOf : Dict ( Int, Int ) ( Int, Int ), deadEnds : Set ( Int, Int ), seed : Random.Seed }
    -> List Point
    -> ( List ( Point, Point ), Random.Seed )
pickPairs doors toPair =
    case List.filter (List.all (allowed doors)) (pairings toPair) of
        first :: others ->
            Random.step (Random.uniform first others) doors.seed

        [] ->
            ( [], doors.seed )


{-| Every way to split a list of doors into pairs. With an odd number, one
door is left over and leads nowhere.
-}
pairings : List Point -> List (List ( Point, Point ))
pairings doors =
    case doors of
        [] ->
            [ [] ]

        [ _ ] ->
            [ [] ]

        first :: rest ->
            rest
                |> List.concatMap
                    (\other ->
                        pairings (List.filter ((/=) other) rest)
                            |> List.map (\more -> ( first, other ) :: more)
                    )


allowed : { a | partOf : Dict ( Int, Int ) ( Int, Int ), deadEnds : Set ( Int, Int ) } -> ( Point, Point ) -> Bool
allowed doors ( a, b ) =
    let
        samePart =
            Dict.get (key a) doors.partOf == Dict.get (key b) doors.partOf

        bothDeadEnds =
            Set.member (key a) doors.deadEnds && Set.member (key b) doors.deadEnds
    in
    not samePart && not bothDeadEnds



-- PARTS OF THE HALL


key : Point -> ( Int, Int )
key { x, y } =
    ( x, y )


{-| Which part of the hall a door opens into, named by the part's top-left
tile: everything Alice can walk to from the floor in front of the door
without using a door.
-}
part : Map -> Point -> ( Int, Int )
part map door =
    case Map.inFrontOf door map of
        Just front ->
            spread map [ front ] (Set.singleton (key front))
                |> Set.toList
                |> List.sortBy (\( x, y ) -> ( y, x ))
                |> List.head
                |> Maybe.withDefault (key door)

        Nothing ->
            key door


spread : Map -> List Point -> Set ( Int, Int ) -> Set ( Int, Int )
spread map todo seen =
    case todo of
        point :: rest ->
            let
                next =
                    Map.directions
                        |> List.map (\direction -> Map.neighbour direction point)
                        |> List.filter
                            (\p ->
                                let
                                    tile =
                                        Map.tileAt p map
                                in
                                Tiles.isWalkable tile && tile /= Portal && not (Set.member (key p) seen)
                            )
            in
            spread map (next ++ rest) (List.foldl (\p s -> Set.insert (key p) s) seen next)

        [] ->
            seen
