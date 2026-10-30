# Helix in Wonderland

A browser game that teaches the [Helix](https://helix-editor.com) editor and
ten-finger typing to beginners, set in Lewis Carroll's *Alice's Adventures in
Wonderland* (1865, public domain). English and German (Swiss spelling, no ß).
Keyboards: German and Swiss QWERTZ, US QWERTY, French AZERTY.

| # | Chapter | Teaches |
|---|---------|---------|
| 1 | Down the Rabbit Hole | the home row, finger by finger (catch jars by their letter) |
| 2 | The Hall of Doors | Helix movement with `h j k l`, and counts like `5j` |
| 3 | Drink Me | typing whole words from memory, the keys `e` and `i`, the space bar |
| 4 | Curiouser and Curiouser | Helix's jumps: `gh gl gs gj gk g.` (squash the currants) |
| 5 | The Pool of Tears | select first, then act: `w x`, then `d`; `u` undoes; counts like `3w`, `2x`; `gh` to the line start |
| 6 | Advice from a Caterpillar | Insert mode: `i`, `a`, `c`, and Esc back to Normal |

Each chapter earns up to three tarts. A chapter opens once the one before it
has at least one; a button on the title screen opens everything for grown-ups.

Written in [Elm](https://elm-lang.org). Want to draw pictures for the game?
See [ART.md](ART.md): there is a pixel editor that turns drawings into
GitHub proposals.

## Commands

```sh
npm install
npm run build    # → dist/helix-wonderland.html, one self-contained page
npm test         # unit tests, fuzz tests and chapter playthroughs
npm run review   # static checks: unused code, leftover Debug.log, simplifications
npm run format   # elm-format everything
npm run check    # all of the above except build; run before every commit
```

`npm install` also installs a git pre-push hook (`.githooks/pre-push`) that
checks formatting and runs the tests before every push. Every pull request is
checked again on GitHub (format, elm-review, tests, build), and `main` only
accepts pull requests whose check passed.

`npm run build` writes two outputs: `dist/site/` is a complete static website
(open `dist/site/index.html` in a browser, or host it anywhere), and
`dist/helix-wonderland.html` is the same game as a page fragment for
claude.ai artifacts.

Pushing to `main` on GitHub runs the tests and publishes `dist/site` to
GitHub Pages (`.github/workflows/pages.yml`). `netlify.toml` does the same
for Netlify if the repo is connected there.

## Make your own copy

Fork the repository on GitHub, then in the fork: **Settings → Pages →
Source: GitHub Actions**, and run the "Test and publish" workflow (or push to
`main`). The game appears at `https://<you>.github.io/<repository>/`; a
custom domain set in the same Pages settings works too.

Nothing needs changing for this: the game finds the pixel editor next to
itself, whatever the address, and the build writes the fork's repository
into the page, so "Suggest a change" and pixel-art proposals open issues in
the fork (see `src/Project.elm` and `tools/build.mjs`).

## How the code is organised

```
src/
  Main.elm                  the shell: settings, screens, routing between chapters
  Chapter/Contract.elm      what every chapter receives and may ask for   ← start here
  Chapter/Story.elm         the story → play → story rhythm all chapters share
  Chapter/Id.elm            the list of chapters, in order
  Chapter/RabbitHole.elm    chapter 1   (art in Chapter/RabbitHole/Well.elm)
  Chapter/Hall.elm          chapter 2   (tiles in Chapter/Hall/Tiles.elm)
  Chapter/DrinkMe.elm       chapter 3
  Chapter/Curiouser.elm     chapter 4   (the crumb table in Chapter/Curiouser/Table.elm)
  Chapter/Pool.elm          chapter 5
  Chapter/Caterpillar.elm   chapter 6
  Editor/Core.elm           a pure model of Helix: text, selection, modes, commands
  Editor/Keymap.elm         which key means which editor command
  Editor/Session.elm        one player working on one text: buffer, keystrokes, locks
  Editor/View.elm           draws a buffer the way Helix does
  Keyboard/Layout.elm       layouts, fingers, home row, layout detection
  Keyboard/View.elm         the colour-coded on-screen keyboard
  Texts.elm                 every word the player reads, per language
  Language.elm, Settings.elm, Progress.elm, Speaker.elm
  Pixel/Sprite.elm          pixel art from text; Pixel/Sprites.elm holds the art
  PixelEditor.elm           the pixel editor page for art proposals (see ART.md)
  Ui/                       dialog box, title, results and end cards, checklist, header
  Project.elm               where this copy lives: links to the site and its GitHub repository
  Ports.elm                 the only doors to JavaScript
public/index.html           page shell, CSS, and the JS side of the ports
tests/                      elm-test suites (Playthrough.elm holds shared helpers)
review/                     elm-review configuration
tools/                      build script, plus offline helpers (see below)
```

### The rules that keep it safe to change

1. **Chapters are pure and isolated.** A chapter exposes `init`, `update` and
   `view`. It can't touch ports, storage or another chapter. To make
   something happen outside itself, `update` returns
   `Chapter.Contract.Effect`s (play a sound, show a toast, flash a key,
   finish with a score) and `Main.applyEffect` carries them out. That one
   function is all the glue there is.

2. **Every chapter tells its story the same way.** `Chapter.Story` owns the
   dialog box and the phases (intro, play, outro). A chapter asks it about
   each key first; only when nobody is talking does the key reach the game.

3. **The compiler checks the content.** `Texts` is a record, so every
   language must provide every line. Layouts, chapters, speakers and editor
   commands are custom types, so adding one makes the compiler list every
   `case` that still needs it.

4. **Time and chance are inputs.** Animation runs on `Frame milliseconds`
   messages, never on timers, so nothing fires after its screen is gone.
   Randomness comes from a seed passed in, so the same seed replays the same
   game. Tests use both to play chapters in microseconds.

### Adding a chapter

1. Create `src/Chapter/YourChapter.elm` with `init`, `update : Context -> Msg
   -> Model -> ( Model, List Effect )` and `view : Context -> Model ->
   Contract.View msg`. `Chapter/Hall.elm` (a game) or `Chapter/Pool.elm` (an
   editor puzzle) are good templates.
2. Add its texts as a new record in `Texts.elm`, for both languages.
3. Add it to `Chapter.Id` (type, `all`, `number`, `toKey`).
4. In `Main.elm`, add a case to `Chapter` and to the four functions under
   `-- CHAPTERS`. The compiler will remind you.
5. Add a playthrough test like the ones in `tests/`.

## Testing approach

- **Editor.Core** has example tests that read like Helix sessions
  (`press [ "w", "d" ]`) and fuzz tests for invariants that must hold for
  *any* text and *any* key sequence: the text keeps its final newline, the
  selection stays inside the text, `d` then `u` restores the text.
- **Keyboard.Layout** pins down fingers, home rows and layout detection for
  all four layouts.
- **Playthroughs** play each chapter through messages, the way a child
  would: a perfect run earns three tarts, mistakes cost tarts, locked keys
  stay locked. If these pass after a refactor, the chapters still work.

## Conventions

- German texts use Swiss spelling: `ss`, never `ß`.
- Keys are handled by the character they type (`KeyboardEvent.key`), so
  Helix commands work on every layout. Layouts only affect teaching: the
  on-screen keyboard, finger hints, and chapter 1's home-row stages.
- Instructions wait until they are relevant. While a story plays nothing
  can be typed, so a chapter shows no instructions then: its `view` goes
  through `Chapter.Story.instructions`, which empties the panel and the
  keyboard hints during the opening story. Show
  each hint at the moment the player can act on it (the difficulty question
  after the story, the movement keys once there is a hall to walk in).
- A chapter's puzzle text (words, poem) follows the language setting until
  the first key press, then stays fixed for that chapter.
- Saved data keys (`Chapter.Id.toKey`, `Keyboard.Layout.toKey`) must never
  change, or players lose their progress. `public/index.html` carries over
  progress saved by the first single-file prototype.
- Story text comes from the 1865 book or is newly written; German lines are
  our own translations. Characters are drawn from the book, never from films.

## Working without package.elm-lang.org

Some sandboxes can reach GitHub and npm but not Elm's package site. Then:

```sh
python3 tools/fetch_elm_packages.py elm/core elm/browser elm/html elm/json elm/svg elm/random elm-explorations/test
python3 tools/make_elm_registry.py
```

The first clones packages at their release tags into `~/.elm`; the second
writes the package index the compiler expects. On a normal machine you never
need these.
