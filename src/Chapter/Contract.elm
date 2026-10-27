module Chapter.Contract exposing
    ( Context, Effect(..), Sound(..), soundName
    , Score, Stat, View, Status, KeyboardFocus
    )

{-| What every chapter receives and what it may ask for.

A chapter is a pure Elm module with `init`, `update` and `view`. It never
touches ports, storage or other chapters. Instead, `update` returns a list of
`Effect`s, and `Main` carries them out. This is the only way out of a chapter,
which keeps the glue between chapters in one place.

@docs Context, Effect, Sound, soundName
@docs Score, Stat, View, Status, KeyboardFocus

-}

import Html exposing (Html)
import Keyboard.Layout exposing (Layout)
import Texts exposing (Texts)


{-| Settings a chapter may read but not change.
-}
type alias Context =
    { texts : Texts
    , layout : Layout
    }


type Effect
    = PlaySound Sound
    | ShowToast (Texts -> String)
    | FlashKey String Bool
    | Finish Score


type Sound
    = StepSound
    | BumpSound
    | PickupSound
    | TickSound
    | CorrectSound
    | ErrorSound
    | PortalSound


{-| The name the JavaScript side knows the sound by.
-}
soundName : Sound -> String
soundName sound =
    case sound of
        StepSound ->
            "step"

        BumpSound ->
            "bump"

        PickupSound ->
            "pickup"

        TickSound ->
            "tick"

        CorrectSound ->
            "correct"

        PortalSound ->
            "portal"

        ErrorSound ->
            "error"


type alias Score =
    { tarts : Int
    , stats : List Stat
    }


type alias Stat =
    { label : Texts -> String
    , value : Texts -> String
    }


{-| A chapter fills the stage, the panel below it, the status line and the
keyboard hints. `Main` arranges them on the page.
-}
type alias View msg =
    { stage : Html msg
    , panel : Html msg
    , status : Status
    , keyboard : KeyboardFocus
    }


{-| Helix's status line: mode, file name, keys waiting for a command, cursor.
-}
type alias Status =
    { mode : String
    , file : String
    , pendingKeys : String
    , position : String
    }


{-| `target` glows on the on-screen keyboard; `helpers` get an outline.
-}
type alias KeyboardFocus =
    { target : Maybe Char
    , helpers : List String
    }
