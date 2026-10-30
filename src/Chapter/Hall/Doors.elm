module Chapter.Hall.Doors exposing
    ( Doors, init
    , leadsTo, mightLeadTo, walkThrough
    , toTheCurtain, steerAway
    , pairs, oneWay
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

With an odd number of doors one is left over. It leads one way, to a
random door of another part. Once Alice goes through it, it and the door
she comes out of are the pair that stays linked, so the way back still
works, and the door that lost its partner is the new one left over.

The randomness comes from a seed, so the same seed plays the same game.

@docs Doors, init
@docs leadsTo, mightLeadTo, walkThrough
@docs toTheCurtain, steerAway
@docs pairs, oneWay

-}

import Chapter.Hall.Map as Map exposing (Map, Point)
import Chapter.Hall.Tiles as Tiles exposing (Tile(..))
import Dict exposing (Dict)
import Random
import Set exposing (Set)


type Doors
    = Doors
        { pairs : List ( Point, Point )
        , oneWay : Maybe ( Point, Point )
        , kept : Maybe ( Point, Point )
        , partOf : Dict ( Int, Int ) ( Int, Int )
        , deadEnds : Set ( Int, Int )
        , curtainPart : Maybe ( Int, Int )
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
            { pairs = []
            , oneWay = Nothing
            , kept = Nothing
            , partOf = partOf
            , deadEnds = deadEnds
            , curtainPart = Map.find CurtainFront map |> Maybe.map (partFrom map)
            , seed = seed
            }
    in
    Doors (link anyWayWill doors empty)


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
        |> orElse
            (case doors.oneWay of
                Just ( from, to ) ->
                    if from == door then
                        Just to

                    else
                        Nothing

                Nothing ->
                    Nothing
            )


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback first =
    case first of
        Just _ ->
            first

        Nothing ->
            fallback


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


{-| Alice went through `door`: it and the door she came out of stay
linked both ways, every other door gets a new partner.
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

                relinked =
                    link anyWayWill others doors
            in
            Doors { relinked | pairs = ( door, partner ) :: relinked.pairs, kept = Just ( door, partner ) }

        Nothing ->
            Doors doors


{-| A door in Alice's part of the hall, other than the way back, that
leads into the curtain's part right now. Chapter 2 gives such a door a
hidden few seconds before it steers away (`steerAway`), so only a quick
player gets through.
-}
toTheCurtain : Map -> Point -> Doors -> Maybe Point
toTheCurtain map alice (Doors doors) =
    let
        here =
            partFrom map alice

        isWayBack door =
            case doors.kept of
                Just ( a, b ) ->
                    door == a || door == b

                Nothing ->
                    False

        intoCurtainPart door =
            case leadsTo door (Doors doors) of
                Just other ->
                    doors.curtainPart /= Nothing && Dict.get (key other) doors.partOf == doors.curtainPart

                Nothing ->
                    False
    in
    Dict.toList doors.partOf
        |> List.filter (\( _, doorPart ) -> doorPart == here)
        |> List.map (\( ( x, y ), _ ) -> { x = x, y = y })
        |> List.filter (\door -> not (isWayBack door) && intoCurtainPart door)
        |> List.head


{-| Link all doors except the way back again, so that `door` no longer
leads into the curtain's part. If there is no such way, nothing changes.
-}
steerAway : Point -> Doors -> Doors
steerAway door (Doors doors) =
    let
        wayBack =
            case doors.kept of
                Just ( a, b ) ->
                    [ a, b ]

                Nothing ->
                    []

        others =
            Dict.keys doors.partOf
                |> List.map (\( x, y ) -> { x = x, y = y })
                |> List.filter (\other -> not (List.member other wayBack))

        avoidsCurtain ( newPairs, newOneWay ) =
            let
                target =
                    newPairs
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
                        |> (\paired ->
                                case paired of
                                    Just _ ->
                                        paired

                                    Nothing ->
                                        newOneWay
                                            |> Maybe.andThen
                                                (\( from, to ) ->
                                                    if from == door then
                                                        Just to

                                                    else
                                                        Nothing
                                                )
                           )
            in
            case target of
                Just other ->
                    Dict.get (key other) doors.partOf /= doors.curtainPart

                Nothing ->
                    True

        relinked =
            link avoidsCurtain others doors

        keptPair =
            case doors.kept of
                Just pair ->
                    [ pair ]

                Nothing ->
                    []
    in
    -- `link` leaves everything as it was (seed too) when no way avoids the
    -- curtain.
    if relinked.seed == doors.seed then
        Doors doors

    else
        Doors { relinked | pairs = keptPair ++ relinked.pairs }


{-| All current pairs, for tests.
-}
pairs : Doors -> List ( Point, Point )
pairs (Doors doors) =
    doors.pairs


{-| The door left over when there is an odd number, and where it leads.
-}
oneWay : Doors -> Maybe ( Point, Point )
oneWay (Doors doors) =
    doors.oneWay



-- PAIRING


type alias Links a =
    { a
        | pairs : List ( Point, Point )
        , oneWay : Maybe ( Point, Point )
        , partOf : Dict ( Int, Int ) ( Int, Int )
        , deadEnds : Set ( Int, Int )
        , seed : Random.Seed
    }


{-| Link up `toLink` at random, in one of the allowed ways: in pairs, and
with an odd number one door left over that leads one way. There are few
doors, so it simply lists every way and picks one. If there is no allowed
way, the doors keep their links.
-}
link : (( List ( Point, Point ), Maybe ( Point, Point ) ) -> Bool) -> List Point -> Links a -> Links a
link acceptable toLink doors =
    let
        everyWay =
            if modBy 2 (List.length toLink) == 0 then
                List.map (\pairing -> ( pairing, Nothing )) (pairings toLink)

            else
                toLink
                    |> List.concatMap
                        (\leftOver ->
                            pairings (List.filter ((/=) leftOver) toLink)
                                |> List.concatMap
                                    (\pairing ->
                                        anyAllowed leftOver doors
                                            |> List.map (\target -> ( pairing, Just ( leftOver, target ) ))
                                    )
                        )

        allowedWays =
            List.filter (\(( pairing, _ ) as way) -> List.all (allowed doors) pairing && acceptable way) everyWay
    in
    case allowedWays of
        first :: others ->
            let
                ( ( newPairs, newOneWay ), nextSeed ) =
                    Random.step (Random.uniform first others) doors.seed
            in
            { doors | pairs = newPairs, oneWay = newOneWay, seed = nextSeed }

        [] ->
            doors


anyWayWill : ( List ( Point, Point ), Maybe ( Point, Point ) ) -> Bool
anyWayWill _ =
    True


{-| Every way to split an even number of doors into pairs.
-}
pairings : List Point -> List (List ( Point, Point ))
pairings doors =
    case doors of
        first :: rest ->
            rest
                |> List.concatMap
                    (\other ->
                        pairings (List.filter ((/=) other) rest)
                            |> List.map (\more -> ( first, other ) :: more)
                    )

        [] ->
            [ [] ]


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
            partFrom map front

        Nothing ->
            key door


{-| The part of the hall a floor tile belongs to, named by its top-left
tile.
-}
partFrom : Map -> Point -> ( Int, Int )
partFrom map point =
    spread map [ point ] (Set.singleton (key point))
        |> Set.toList
        |> List.sortBy (\( x, y ) -> ( y, x ))
        |> List.head
        |> Maybe.withDefault (key point)


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
