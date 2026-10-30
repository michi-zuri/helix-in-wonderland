module Editor.Session exposing
    ( Session, start, fromBuffer, buffer, keystrokes, recentKeys, pendingCount, isWaiting, jumpLabels
    , Outcome(..), press, splash, replaceChar, matches, matchingLines
    )

{-| One player working on one text: the buffer plus what the status line
and the score need (keystrokes, the last few keys).

Chapters decide which commands are unlocked by passing a predicate to
`press`. A locked command is reported, not run, so the chapter can explain
that it comes later.

In Normal mode, digits before a command are a count, as in Helix: `3x`
selects three lines, `5w` the fifth word ahead (see `Core.applyTimes`). A
count can't start with 0, and Esc or any key that isn't a command drops it.

`g` starts a two-key command (Helix's goto mode): `gh` goes to the start of
the line, `gl` to its end, and so on (see `Keymap.gotoCommand`). Any other
key after `g` cancels it.

`gw` puts a two-letter label on every word (`jumpLabels`); typing a label
selects that word, as in Helix. Esc or a key that matches no label cancels.

@docs Session, start, fromBuffer, buffer, keystrokes, recentKeys, pendingCount, isWaiting, jumpLabels
@docs Outcome, press, splash, replaceChar, matches, matchingLines

-}

import Editor.Core as Core exposing (Buffer, Command(..), Mode(..))
import Editor.Keymap as Keymap


type Session
    = Session
        { buffer : Buffer
        , keystrokes : Int
        , recentKeys : List String
        , count : String
        , afterG : Bool
        , jump : Maybe Jump
        }


{-| `gw` in progress: the labels, and the first letter once it is typed.
-}
type alias Jump =
    { labels : List ( String, { anchor : Int, head : Int } )
    , typed : String
    }


start : String -> Session
start text =
    fromBuffer (Core.fromString text)


{-| Start from a buffer set up already, for example with soft wrap.
-}
fromBuffer : Buffer -> Session
fromBuffer startingBuffer =
    Session { buffer = startingBuffer, keystrokes = 0, recentKeys = [], count = "", afterG = False, jump = Nothing }


{-| The labels `gw` shows, at the start of each word they belong to. Once
the first letter is typed, only the labels starting with it remain.
-}
jumpLabels : Session -> List { position : Int, label : String }
jumpLabels (Session s) =
    case s.jump of
        Just jump ->
            jump.labels
                |> List.filter (\( label, _ ) -> String.startsWith jump.typed label)
                |> List.map (\( label, range ) -> { position = range.anchor, label = label })

        Nothing ->
            []


labelLetters : List Char
labelLetters =
    String.toList "abcdefghijklmnopqrstuvwxyz"


{-| "aa", "ab", ... one label per word, as many as two letters allow.
-}
labelsFor : Buffer -> List ( String, { anchor : Int, head : Int } )
labelsFor current =
    let
        names =
            labelLetters |> List.concatMap (\first -> List.map (\second -> String.fromList [ first, second ]) labelLetters)
    in
    List.map2 Tuple.pair names (Core.wordStarts current)


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


{-| Is a count, a `g` or a `gw` waiting for the rest of its command?
-}
isWaiting : Session -> Bool
isWaiting (Session s) =
    s.count /= "" || s.afterG || s.jump /= Nothing


{-| `Waiting` is a key that starts a command without finishing it: a digit
of a count, or `g`. The command after a count runs with that count (`count`
is 1 without one).
-}
type Outcome
    = Ran { command : Command, count : Int, before : Buffer, session : Session }
    | Waiting Session
    | Locked Session
    | Ignored Session


press : (Command -> Bool) -> String -> Session -> Outcome
press isUnlocked key (Session s) =
    case s.jump of
        Just jump ->
            pickLabel key jump (Session { s | jump = Nothing })

        Nothing ->
            pressNormally isUnlocked key (Session s)


pressNormally : (Command -> Bool) -> String -> Session -> Outcome
pressNormally isUnlocked key (Session s) =
    let
        mode =
            Core.mode s.buffer

        waitFor next =
            Waiting
                (Session
                    { next
                        | keystrokes = s.keystrokes + 1
                        , recentKeys = remember mode key s.recentKeys
                    }
                )
    in
    if mode == Normal && s.afterG && Keymap.gotoCommand key == Just GotoWord then
        if isUnlocked GotoWord then
            waitFor { s | afterG = False, count = "", jump = Just { labels = labelsFor s.buffer, typed = "" } }

        else
            Locked (Session { s | afterG = False, count = "" })

    else if mode == Normal && s.afterG then
        -- The second key of a goto command; like Helix, it ignores a count.
        run isUnlocked key (Session { s | afterG = False, count = "" }) (Keymap.gotoCommand key)

    else if mode == Normal && key == "g" then
        waitFor { s | afterG = True }

    else if mode == Normal && isCountDigit key s.count then
        waitFor { s | count = s.count ++ key }

    else
        run isUnlocked key (Session s) (Keymap.commandFor mode key)


{-| A letter typed while `gw` shows its labels. The first narrows them
down, the second picks a word; anything else cancels.
-}
pickLabel : String -> Jump -> Session -> Outcome
pickLabel key jump (Session s) =
    let
        typed =
            jump.typed ++ key

        matching =
            List.filter (\( label, _ ) -> String.startsWith typed label) jump.labels
    in
    case ( String.length typed, matching ) of
        ( 1, _ :: _ ) ->
            Waiting
                (Session
                    { s
                        | jump = Just { jump | typed = typed }
                        , keystrokes = s.keystrokes + 1
                        , recentKeys = remember Normal key s.recentKeys
                    }
                )

        ( 2, [ ( _, range ) ] ) ->
            run (always True) key (Session s) (Just (SelectRange range))

        _ ->
            Ignored (Session s)


{-| Run `maybeCommand` (the command `key` finished), if it is unlocked.
-}
run : (Command -> Bool) -> String -> Session -> Maybe Command -> Outcome
run isUnlocked key (Session s) maybeCommand =
    let
        mode =
            Core.mode s.buffer

        withoutCount =
            Session { s | count = "", afterG = False, jump = Nothing }
    in
    case maybeCommand of
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
                            , afterG = False
                            , jump = Nothing
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


{-| One character changed from outside, as a game does (see
`Core.replaceChar`); `g.` goes back there.
-}
replaceChar : Int -> Char -> Session -> Session
replaceChar position char (Session s) =
    Session { s | buffer = Core.replaceChar position char s.buffer }


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
