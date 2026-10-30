module Chapter.Id exposing (ChapterId(..), all, next, number, previous, toKey)

{-| The chapters of the game, in playing order.
-}


type ChapterId
    = RabbitHole
    | HallOfDoors
    | DrinkMe
    | Curiouser
    | PoolOfTears
    | CaterpillarsAdvice


all : List ChapterId
all =
    [ RabbitHole, HallOfDoors, DrinkMe, Curiouser, PoolOfTears, CaterpillarsAdvice ]


{-| 1-based, as shown on the title screen.
-}
number : ChapterId -> Int
number id =
    case id of
        RabbitHole ->
            1

        HallOfDoors ->
            2

        DrinkMe ->
            3

        Curiouser ->
            4

        PoolOfTears ->
            5

        CaterpillarsAdvice ->
            6


{-| Stable name for saving progress. Never change these: saved games use them.
-}
toKey : ChapterId -> String
toKey id =
    case id of
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
