module ProjectTest exposing (suite)

{-| The game's links work on any copy of it: at a domain's root, under a
repository path, and in a fork.
-}

import Expect
import Json.Decode as Decode
import Project
import Test exposing (Test, describe, test)


atRoot : Project.Links
atRoot =
    { site = "https://helix.example.com/", repository = "SomeKid/helix-fork" }


underPath : Project.Links
underPath =
    { site = "https://somekid.github.io/helix-fork/", repository = "SomeKid/helix-fork" }


suite : Test
suite =
    describe "links to the site and its repository"
        [ test "the pixel editor sits next to the game, at a domain's root" <|
            \_ -> Project.pixelEditorUrl atRoot |> Expect.equal "https://helix.example.com/pixel-editor.html"
        , test "the pixel editor sits next to the game, under a repository path" <|
            \_ -> Project.pixelEditorUrl underPath |> Expect.equal "https://somekid.github.io/helix-fork/pixel-editor.html"
        , test "a site address without a final slash still works" <|
            \_ -> Project.gameUrl { underPath | site = "https://somekid.github.io/helix-fork" } |> Expect.equal "https://somekid.github.io/helix-fork/"
        , test "a change proposal opens the fork's form and names the screen" <|
            \_ ->
                Project.proposeEditUrl atRoot "Chapter 3: Drink Me (DE)"
                    |> Expect.equal "https://github.com/SomeKid/helix-fork/issues/new?template=change-proposal.yml&title=Change%3A%20Chapter%203%3A%20Drink%20Me%20(DE)&where=Chapter%203%3A%20Drink%20Me%20(DE)"
        , test "when the page passes nothing, links stay relative and go to the original repository" <|
            \_ ->
                Decode.decodeString Project.decoder "{}"
                    |> Result.map (\links -> ( Project.pixelEditorUrl links, Project.newIssueUrl links ))
                    |> Expect.equal (Ok ( "./pixel-editor.html", "https://github.com/michi-zuri/helix-in-wonderland/issues/new" ))
        ]
