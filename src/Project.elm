module Project exposing (gameUrl, newIssueUrl, pixelEditorUrl, proposeEditUrl)

{-| Where the project lives online. Change these if the repository or the
site moves; every link in the game and the pixel editor comes from here.
-}

import Url


repository : String
repository =
    "https://github.com/michi-zuri/helix-in-wonderland"


site : String
site =
    "https://michi-zuri.github.io/helix-in-wonderland/"


gameUrl : String
gameUrl =
    site


pixelEditorUrl : String
pixelEditorUrl =
    site ++ "pixel-editor.html"


newIssueUrl : String
newIssueUrl =
    repository ++ "/issues/new"


{-| A new GitHub issue with the "Change proposal" form, already saying which
screen it is about (see `.github/ISSUE_TEMPLATE/change-proposal.yml`).
-}
proposeEditUrl : String -> String
proposeEditUrl screen =
    newIssueUrl
        ++ "?template=change-proposal.yml"
        ++ "&title="
        ++ Url.percentEncode ("Change: " ++ screen)
        ++ "&where="
        ++ Url.percentEncode screen
