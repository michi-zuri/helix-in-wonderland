module ProjectTest exposing (suite)

import Expect
import Project
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "links to the project"
        [ test "a change proposal opens the change form and names the screen" <|
            \_ ->
                Project.proposeEditUrl "Chapter 3: Drink Me (DE)"
                    |> Expect.equal "https://github.com/michi-zuri/helix-in-wonderland/issues/new?template=change-proposal.yml&title=Change%3A%20Chapter%203%3A%20Drink%20Me%20(DE)&where=Chapter%203%3A%20Drink%20Me%20(DE)"
        , test "the pixel editor lives next to the game" <|
            \_ -> Project.pixelEditorUrl |> Expect.equal (Project.gameUrl ++ "pixel-editor.html")
        ]
