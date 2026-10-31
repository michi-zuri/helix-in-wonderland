module Chapter.Id exposing (ChapterId(..), all, next, number, previous, toKey)

{-| The chapters of the game, in playing order.
-}


type ChapterId
    = RabbitHoleRightHand
    | HallOfDoors
    | RabbitHole
    | DrinkMe
    | Curiouser
    | PoolOfTears
    | CaterpillarsAdvice


all : List ChapterId
all =
    [ RabbitHoleRightHand, HallOfDoors, RabbitHole, DrinkMe, Curiouser, PoolOfTears, CaterpillarsAdvice ]


{-| 1-based, as shown on the title screen.
-}
number : ChapterId -> Int
number id =
    case id of
        RabbitHoleRightHand ->
            1

        HallOfDoors ->
            2

        RabbitHole ->
            3

        DrinkMe ->
            4

        Curiouser ->
            5

        PoolOfTears ->
            6

        CaterpillarsAdvice ->
            7


{-| Stable name for saving progress. Never change these: saved games use them.
-}
toKey : ChapterId -> String
toKey id =
    case id of
        RabbitHoleRightHand ->
            "rabbit-hole-right-hand"

        RabbitHole ->
            "rabbit-hole"

        HallOfDoors ->
            "hall-of-doors"

        DrinkMe ->
            "drink-me"

        Curiouser ->
            "curiouser"

        PoolOfTears ->
            "pool-of-tears"

        CaterpillarsAdvice ->
            "caterpillar"


next : ChapterId -> Maybe ChapterId
next id =
    all
        |> List.filter (\other -> number other == number id + 1)
        |> List.head


previous : ChapterId -> Maybe ChapterId
previous id =
    all
        |> List.filter (\other -> number other == number id - 1)
        |> List.head
