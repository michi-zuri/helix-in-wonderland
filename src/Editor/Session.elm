module Editor.Session exposing
    ( Session, start, buffer, keystrokes, recentKeys, pendingCount
    , Outcome(..), press, splash, matches, matchingLines
    )

{-| One player working on one text: the buffer plus what the status line
and the score need (keystrokes, the last few keys).

Chapters decide which commands are unlocked by passing a predicate to
`press`. A locked command is reported, not run, so the chapter can explain
that it comes later.

In Normal mode, digits before a command are a count, as in Helix: `3x`
selects three lines, `5w` the fifth word ahead (see `Core.applyTimes`). A
count can't start with 0, and Esc or any key that isn't a command drops it.

@docs Session, start, buffer, keystrokes, recentKeys, pendingCount
@docs Outcome, press, splash, matches, matchingLines

-}

import Editor.Core as Core exposing (Buffer, Command, Mode(..))
import Editor.Keymap as Keymap


type Session
    = Session
        { buffer : Buffer
        , keystrokes : Int
        , recentKeys : List String
        , count : String
        }


start : String -> Session
start text =
    Session { buffer = Core.fromString text, keystrokes = 0, recentKeys = [], count = "" }


buffer : Session -> Buffer
buffer (Session s) =
    s.buffer


keystrokes : Session -> Int
keystrokes (Session s) =
    s.keystrokes


{-| The last Normal-mode keys, oldest first, as Helix shows them in its
status line.
-}
recentKeys : Session -> List String
recentKeys (Session s) =
    List.reverse s.recentKeys


{-| The count typed so far, `""` when there is none: Helix shows it in the
status line.
-}
pendingCount : Session -> String
pendingCount (Session s) =
    s.count


{-| `Counting` is a digit added to the count; the command after it runs
with that count (`count` is 1 without one).
-}
type Outcome
    = Ran { command : Command, count : Int, before : Buffer, session : Session }
    | Counting Session
    | Locked Session
    | Ignored Session


press : (Command -> Bool) -> String -> Session -> Outcome
press isUnlocked key (Session s) =
    let
        mode =
            Core.mode s.buffer

        withoutCount =
            Session { s | count = "" }
    in
    if mode == Normal && isCountDigit key s.count then
        Counting
            (Session
                { s
                    | count = s.count ++ key
                    , keystrokes = s.keystrokes + 1
                    , recentKeys = remember mode key s.recentKeys
                }
            )

    else
        case Keymap.commandFor mode key of
            Just command ->
                if isUnlocked command then
                    let
                        count =
                            String.toInt s.count |> Maybe.withDefault 1
                    in
                    Ran
                        { command = command
                        , count = count
                        , before = s.buffer
                        , session =
                            Session
                                { buffer = Core.applyTimes count command s.buffer
                                , keystrokes = s.keystrokes + 1
                                , recentKeys = remember mode key s.recentKeys
                                , count = ""
                                }
                        }

                else
                    Locked withoutCount

            Nothing ->
                Ignored withoutCount


{-| A word put in from outside, not typed (see `Core.splash`).
-}
splash : { line : Int, word : Int, text : String } -> Session -> Session
splash where_ (Session s) =
    Session { s | buffer = Core.splash where_ s.buffer }


{-| A count starts with 1-9; after that 0 is fine too.
-}
isCountDigit : String -> String -> Bool
isCountDigit key count =
    case String.toList key of
        [ char ] ->
            Char.isDigit char && (char /= '0' || count /= "")

        _ ->
            False


remember : Mode -> String -> List String -> List String
remember mode key recent =
    case mode of
        Normal ->
            List.take 5
                ((if key == "Escape" then
                    "esc"

                  else
                    key
                 )
                    :: recent
                )

        Insert ->
            recent


{-| Does the text read like the target? Extra spaces, spaces at the ends of
lines and empty lines don't count, so a child who deletes a word with `e`
(leaving two spaces) has still done it right.
-}
matches : String -> Session -> Bool
matches target session =
    normalize (Core.toString (buffer session)) == normalize target


{-| How many lines of the target already read right, line by line.
-}
matchingLines : String -> Session -> Int
matchingLines target session =
    List.map2 (==) (normalize target) (normalize (Core.toString (buffer session)))
        |> List.filter identity
        |> List.length


normalize : String -> List String
normalize text =
    String.lines text
        |> List.map (String.words >> String.join " ")
        |> List.filter (not << String.isEmpty)
