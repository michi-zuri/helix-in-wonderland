# Drawing pictures for the game

*Deutsch weiter unten.*

All characters and props in Helix in Wonderland are pixel art, and you can
suggest new ones or improve the old ones.

## The quick way: the pixel editor

1. Open the **pixel editor**: click **Draw pictures for the game** at the bottom
   of any screen in the game. (It is the page `pixel-editor.html` next to the
   game, for example <https://michi-zuri.github.io/helix-in-wonderland/pixel-editor.html>.)
2. Pick a picture at the top (for example `tart` or `caterpillar`).
3. Choose a colour on the right and paint by clicking or dragging on the big grid.
   - The `.` button is the eraser: it makes a pixel see-through.
   - Click the little colour box next to a letter to change that colour everywhere.
   - "+ new colour" adds a colour with a new letter.
4. Watch the small preview: that's the size it will roughly have in the game.
5. Click **Send as a proposal on GitHub**. A form opens with your picture
   already filled in. Write what you changed, then click "Submit new issue".
   (You need a free GitHub account for this step. Ask a grown-up if you don't
   have one.)

Someone then looks at your proposal. If it goes into the game, it becomes a
change to the code that you can see on GitHub.

## Where the pictures live

The pictures are written as text in **`src/Pixel/Sprites.elm`**. Each one
looks like this:

```elm
tart =
    Sprite.fromRows
        [ ( 'c', "#f4c15d" ) -- crust
        , ( 'j', "#e8203f" ) -- jam
        ]
        [ "..cccc.."
        , ".cjjjjc."
        ]
```

- The first list says which **letter means which colour** (colours are
  written as `#rrggbb`).
- The second list is the **picture**: one line of text per row of pixels,
  one letter per pixel. A `.` means "nothing here".

Some things in the game are drawn with code instead of pictures: the tiles
of the hall (`src/Chapter/Hall/Tiles.elm`) and the walls of the rabbit hole
(`src/Chapter/RabbitHole/Well.elm`). For those, draw your idea in the
editor as a new picture, or describe it in an issue.

## Rules for game pictures

- Keep the size the same as the old picture, unless you're making something new.
- Draw from the book, *Alice's Adventures in Wonderland* (1865), not from films.
- Letters must be the same in both lists: every letter in the picture needs a colour.

---

# Bilder für das Spiel zeichnen

Alle Figuren und Gegenstände in Helix im Wunderland sind Pixel-Bilder. Du
kannst neue vorschlagen oder alte verbessern.

## Am schnellsten: der Pixel-Editor

1. Öffne den **Pixel-Editor**: klick unten auf irgendeiner Seite im Spiel auf
   **Bilder fürs Spiel zeichnen**. (Das ist die Seite `pixel-editor.html` neben
   dem Spiel, zum Beispiel <https://michi-zuri.github.io/helix-in-wonderland/pixel-editor.html>.)
2. Wähl oben ein Bild aus (zum Beispiel `tart` oder `caterpillar`).
3. Wähl rechts eine Farbe und mal mit Klicken oder Ziehen auf dem grossen Raster.
   - Der Knopf `.` ist der Radierer: der Pixel wird durchsichtig.
   - Klick auf das kleine Farbfeld neben einem Buchstaben, um diese Farbe überall zu ändern.
   - "+ neue Farbe" fügt eine Farbe mit einem neuen Buchstaben hinzu.
4. Schau auf die kleine Vorschau: so gross ist das Bild ungefähr im Spiel.
5. Klick auf **Als Vorschlag auf GitHub schicken**. Ein Formular geht auf,
   dein Bild ist schon eingetragen. Schreib, was du geändert hast, und klick
   auf "Submit new issue". (Dafür brauchst du ein kostenloses GitHub-Konto.
   Frag eine erwachsene Person, wenn du keins hast.)

Dann schaut sich jemand deinen Vorschlag an. Wenn er ins Spiel kommt, wird
daraus eine Änderung am Code, die du auf GitHub sehen kannst.

## Wo die Bilder wohnen

Die Bilder stehen als Text in **`src/Pixel/Sprites.elm`**. Die erste Liste
sagt, **welcher Buchstabe welche Farbe** ist (Farben als `#rrggbb`). Die
zweite Liste ist das **Bild**: eine Textzeile pro Pixelreihe, ein Buchstabe
pro Pixel. Ein `.` heisst "hier ist nichts".

Manches im Spiel ist mit Code gezeichnet statt mit Bildern: die Fliesen im
Saal (`src/Chapter/Hall/Tiles.elm`) und die Wände im Kaninchenbau
(`src/Chapter/RabbitHole/Well.elm`). Dafür zeichnest du deine Idee im
Editor als neues Bild oder beschreibst sie in einem Issue.

## Regeln für Spielbilder

- Lass die Grösse gleich wie beim alten Bild, ausser du machst etwas Neues.
- Zeichne nach dem Buch *Alice im Wunderland* (1865), nicht nach Filmen.
- Jeder Buchstabe im Bild braucht eine Farbe in der ersten Liste.
