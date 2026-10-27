module Project exposing (Links, decoder, fallback, gameUrl, newIssueUrl, pixelEditorUrl, proposeEditUrl)

{-| Where the game lives online, so its links work on any copy of it.

Nothing here is fixed to one address. The page works out both parts when
it starts (see `public/index.html`) and hands them to the game:

  - `site`: the folder the game is served from, taken from the page's own
    address. The pixel editor sits next to the game, so links between them
    work at a domain's root, under `/helix-in-wonderland/`, or anywhere else.
  - `repository`: the GitHub repository the site was built from
    (`owner/name`). The build reads it from GitHub Actions
    (`GITHUB_REPOSITORY`) or from the git remote, so a fork's proposal links
    open issues in the fork.

-}

import Json.Decode as Decode exposing (Decoder)
import Url


type alias Links =
    { site : String
    , repository : String
    }


{-| Used when the page passes nothing: relative links still work wherever
the game is served, and proposals go to the original repository.
-}
fallback : Links
fallback =
    { site = "./", repository = "michi-zuri/helix-in-wonderland" }


decoder : Decoder Links
decoder =
    Decode.map2 Links
        (Decode.oneOf [ Decode.field "site" Decode.string, Decode.succeed fallback.site ])
        (Decode.oneOf [ Decode.field "repository" Decode.string, Decode.succeed fallback.repository ])


gameUrl : Links -> String
gameUrl links =
    withSlash links.site


pixelEditorUrl : Links -> String
pixelEditorUrl links =
    withSlash links.site ++ "pixel-editor.html"


newIssueUrl : Links -> String
newIssueUrl links =
    "https://github.com/" ++ links.repository ++ "/issues/new"


{-| A new GitHub issue with the "Change proposal" form, already saying which
screen it is about (see `.github/ISSUE_TEMPLATE/change-proposal.yml`).
-}
proposeEditUrl : Links -> String -> String
proposeEditUrl links screen =
    newIssueUrl links
        ++ "?template=change-proposal.yml"
        ++ "&title="
        ++ Url.percentEncode ("Change: " ++ screen)
        ++ "&where="
        ++ Url.percentEncode screen


withSlash : String -> String
withSlash site =
    if String.endsWith "/" site then
        site

    else
        site ++ "/"
