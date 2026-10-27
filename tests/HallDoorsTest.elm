module HallDoorsTest exposing (suite)

{-| The mad hall's doors (`Chapter.Hall.Doors`) keep their promises for any
seed and any order of going through doors:

  - every door leads somewhere, and two doors always lead to each other;
  - the way back always works;
  - no pair could trap Alice (two doors of one part of the hall, or two
    dead ends);
  - the other doors really do change.

-}

import Chapter.Hall.Doors as Doors exposing (Doors)
import Chapter.Hall.Map as Map exposing (Map, Point)
import Chapter.Hall.Tiles exposing (Tile(..))
import Difficulty exposing (Difficulty(..))
import Expect
import Fuzz
import Random
import Test exposing (Test, describe, fuzz2, test)


madMap : Map
madMap =
    Map.parse (Map.plan Mad)


allDoors : List Point
allDoors =
    Map.findAll Portal madMap


{-| Which part of the hall each door opens into, written out by hand from
the map so the test doesn't repeat the code it checks.
-}
partOf : Point -> String
partOf door =
    case ( door.x, door.y ) of
        ( 0, 7 ) ->
            "start"

        ( 4, 0 ) ->
            "top left"

        ( 9, 0 ) ->
            "top left"

        ( 14, 0 ) ->
            "key"

        ( 19, 7 ) ->
            "key"

        _ ->
            "curtain"


isDeadEnd : Point -> Bool
isDeadEnd door =
    List.member (partOf door) [ "start", "curtain" ]


{-| Go through the doors picked by `choices` (each one an index into the
doors), and collect the doors after every trip.
-}
journey : Int -> List Int -> List ( Point, Doors )
journey seed choices =
    List.foldl
        (\choice ( doors, trips ) ->
            let
                door =
                    List.drop (modBy (List.length allDoors) choice) allDoors |> List.head |> Maybe.withDefault { x = 0, y = 7 }

                after =
                    Doors.walkThrough door doors
            in
            ( after, ( door, after ) :: trips )
        )
        ( Doors.init (Random.initialSeed seed) madMap, [] )
        choices
        |> Tuple.second


{-| The pairs in a fixed order, so two pairings can be compared.
-}
pairing : Doors -> List ( ( Int, Int ), ( Int, Int ) )
pairing doors =
    Doors.pairs doors
        |> List.map
            (\( a, b ) ->
                if ( a.x, a.y ) < ( b.x, b.y ) then
                    ( ( a.x, a.y ), ( b.x, b.y ) )

                else
                    ( ( b.x, b.y ), ( a.x, a.y ) )
            )
        |> List.sort


problems : Doors -> List String
problems doors =
    let
        pairs =
            Doors.pairs doors

        used =
            List.concatMap (\( a, b ) -> [ a, b ]) pairs
    in
    List.concat
        [ if List.sortBy (\p -> ( p.x, p.y )) used == List.sortBy (\p -> ( p.x, p.y )) allDoors then
            []

          else
            [ "not every door is in exactly one pair" ]
        , pairs
            |> List.filter (\( a, b ) -> partOf a == partOf b)
            |> List.map (\_ -> "two doors of the same part are linked")
        , pairs
            |> List.filter (\( a, b ) -> isDeadEnd a && isDeadEnd b)
            |> List.map (\_ -> "the two dead ends are linked")
        , pairs
            |> List.filter (\( a, b ) -> Doors.leadsTo a doors /= Just b || Doors.leadsTo b doors /= Just a)
            |> List.map (\_ -> "a pair doesn't lead both ways")
        ]


suite : Test
suite =
    describe "The doors of the mad hall"
        [ test "the mad hall has six doors that lead somewhere" <|
            \_ -> List.length allDoors |> Expect.equal 6
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.intRange 0 5)) "every pairing is complete and can't trap Alice" <|
            \seed choices ->
                journey seed choices
                    |> List.concatMap (Tuple.second >> problems)
                    |> Expect.equal []
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.intRange 0 5)) "the way back always leads to the door Alice came from" <|
            \seed choices ->
                journey seed choices
                    |> List.filter
                        (\( door, after ) ->
                            Maybe.andThen (\exit -> Doors.leadsTo exit after) (Doors.leadsTo door after) /= Just door
                        )
                    |> Expect.equal []
        , test "the other doors really do change" <|
            \_ ->
                List.range 1 20
                    |> List.map
                        (\seed ->
                            let
                                before =
                                    Doors.init (Random.initialSeed seed) madMap

                                door =
                                    { x = 0, y = 7 }
                            in
                            pairing before /= pairing (Doors.walkThrough door before |> Doors.walkThrough (Doors.leadsTo door before |> Maybe.withDefault door))
                        )
                    |> List.filter identity
                    |> List.length
                    |> Expect.atLeast 5
        ]
