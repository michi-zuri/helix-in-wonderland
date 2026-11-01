module InstructionsTest exposing (suite)

{-| Instructions wait until they are relevant: while a chapter's opening
story plays nothing can be typed, so the panel below the stage stays empty
(`Chapter.Story.instructions`). Once the story is over, the panel shows what to do
next.
-}

import Chapter.Contract exposing (Context)
import Chapter.Hall as Hall
import Chapter.RabbitHole as RabbitHole
import Keyboard.Layout exposing (Layout(..))
import Language exposing (Language(..))
import Playthrough
import Random
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Texts


context : Context
context =
    { texts = Texts.forLanguage English, layout = UsQwerty, escapeKeys = Nothing }


rabbitHoleAfterStory : RabbitHole.Model
rabbitHoleAfterStory =
    Playthrough.start (RabbitHole.init (Random.initialSeed 1))
        |> Playthrough.sendAll (RabbitHole.update context) (List.repeat 12 (RabbitHole.KeyPressed "Enter"))
        |> .model


hallAfterStory : Hall.Model
hallAfterStory =
    Playthrough.start (Hall.init (Random.initialSeed 1))
        |> Playthrough.sendAll (Hall.update context) (List.repeat 12 (Hall.KeyPressed "Enter"))
        |> .model


suite : Test
suite =
    describe "Instructions wait until they are relevant"
        [ test "chapter 1: no difficulty question while the story plays" <|
            \_ ->
                (RabbitHole.view context (RabbitHole.init (Random.initialSeed 1))).panel
                    |> Query.fromHtml
                    |> Query.hasNot [ text context.texts.rabbitHole.chooseHint ]
        , test "chapter 1: the difficulty question once the story is over" <|
            \_ ->
                (RabbitHole.view context rabbitHoleAfterStory).panel
                    |> Query.fromHtml
                    |> Query.has [ text context.texts.rabbitHole.chooseHint ]
        , test "chapter 2: no task while the story plays" <|
            \_ ->
                (Hall.view context (Hall.init (Random.initialSeed 1))).panel
                    |> Query.fromHtml
                    |> Query.hasNot [ text context.texts.hall.findKey ]
        , test "chapter 2: the difficulty question after the story, before the task" <|
            \_ ->
                (Hall.view context hallAfterStory).panel
                    |> Query.fromHtml
                    |> Query.has [ text context.texts.hall.chooseHint ]
        ]
