module Progress exposing (Progress, decoder, empty, encode, isOpen, record, tartsFor, total)

{-| Tarts earned per chapter. Only the best result for a chapter is kept.
-}

import Chapter.Id as ChapterId exposing (ChapterId)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Progress
    = Progress (Dict String Int)


empty : Progress
empty =
    Progress Dict.empty


tartsFor : ChapterId -> Progress -> Int
tartsFor id (Progress best) =
    Dict.get (ChapterId.toKey id) best |> Maybe.withDefault 0


{-| A chapter opens once the chapter before it has earned at least one tart.
The first chapter is always open, and grown-ups can open everything. A
chapter that already has tarts stays open, and so does every chapter before
a chapter with tarts: when chapters are added or moved, nothing a player has
reached gets locked away.
-}
isOpen : { unlockAll : Bool } -> ChapterId -> Progress -> Bool
isOpen { unlockAll } id progress =
    let
        reachedLater =
            ChapterId.all
                |> List.filter (\other -> ChapterId.number other >= ChapterId.number id)
                |> List.any (\other -> tartsFor other progress > 0)
    in
    case ChapterId.previous id of
        Just earlier ->
            unlockAll || tartsFor earlier progress > 0 || reachedLater

        Nothing ->
            True


total : Progress -> Int
total (Progress best) =
    List.sum (Dict.values best)


record : ChapterId -> Int -> Progress -> Progress
record id tarts ((Progress best) as progress) =
    Progress (Dict.insert (ChapterId.toKey id) (max tarts (tartsFor id progress)) best)


decoder : Decoder Progress
decoder =
    Decode.map Progress (Decode.dict Decode.int)


encode : Progress -> Encode.Value
encode (Progress best) =
    Encode.dict identity Encode.int best
