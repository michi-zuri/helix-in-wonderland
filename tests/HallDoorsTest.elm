module HallDoorsTest exposing (suite)

{-| The mad hall's doors (`Chapter.Hall.Doors`) keep their promises for any
seed and any order of going through doors:

  - every door leads somewhere, every time: the doors are linked in pairs,
    and as there are seven, one more leads one way;
  - the way back always works;
  - no link could trap Alice (two doors of one part of the hall, or two
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

        ( 5, 11 ) ->
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


{-| The only part with a single door.
-}
isDeadEnd : Point -> Bool
isDeadEnd door =
    partOf door == "curtain"


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

        oneWay =
            Doors.oneWay doors

        used =
            List.concatMap (\( a, b ) -> [ a, b ]) pairs ++ (oneWay |> Maybe.map (Tuple.first >> List.singleton) |> Maybe.withDefault [])
    in
    List.concat
        [ if List.sortBy (\p -> ( p.x, p.y )) used == List.sortBy (\p -> ( p.x, p.y )) allDoors then
            []

          else
            [ "not every door is in exactly one pair, or the one door left over" ]
        , allDoors
            |> List.filter (\door -> Doors.leadsTo door doors == Nothing)
            |> List.map (\_ -> "a door leads nowhere")
        , case oneWay of
            Just ( from, to ) ->
                if partOf from == partOf to then
                    [ "the one-way door leads into its own part" ]

                else
                    []

            Nothing ->
                [ "seven doors, but none left over" ]
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
        [ test "all seven doors of the mad hall lead somewhere" <|
            \_ -> List.length allDoors |> Expect.equal 7
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.intRange 0 6)) "every door always leads somewhere, and no link can trap Alice" <|
            \seed choices ->
                journey seed choices
                    |> List.concatMap (Tuple.second >> problems)
                    |> Expect.equal []
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.intRange 0 6)) "the way back always leads to the door Alice came from" <|
            \seed choices ->
                journey seed choices
                    |> List.filter
                        (\( door, after ) ->
                            Maybe.andThen (\exit -> Doors.leadsTo exit after) (Doors.leadsTo door after) /= Just door
                        )
                    |> Expect.equal []
        , fuzz2 Fuzz.int (Fuzz.list (Fuzz.intRange 0 6)) "steering away: the door no longer leads to the curtain, the way back stays, nothing can trap Alice" <|
            \seed choices ->
                journey seed choices
                    |> List.filterMap
                        (\( door, after ) ->
                            let
                                alice =
                                    Doors.leadsTo door after |> Maybe.andThen (\exit -> Map.inFrontOf exit madMap) |> Maybe.withDefault { x = 1, y = 10 }
                            in
                            Doors.toTheCurtain madMap alice after
                                |> Maybe.map
                                    (\toCurtain ->
                                        let
                                            steered =
                                                Doors.steerAway toCurtain after
                                        in
                                        ( Doors.leadsTo toCurtain steered |> Maybe.map partOf
                                        , Doors.leadsTo door steered == Doors.leadsTo door after
                                        , problems steered
                                        )
                                    )
                        )
                    |> List.filter (\( target, wayBackKept, trouble ) -> target == Just "curtain" || not wayBackKept || not (List.isEmpty trouble))
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
