# Working on Helix in Wonderland with Claude

This file is for Claude Code sessions (and anyone else helping). It holds what
README.md doesn't: how the owner likes work done, the design decisions behind
the chapters, and the tricks that made past work go well. Read README.md too;
it has the commands, the code layout and the rules that keep chapters safe to
change. Everything that happened so far is documented issue by issue and pull
request by pull request (from #1 on GitHub): when in doubt why something is the
way it is, read the issue and PR that introduced it.

## Who it's for

A game for the owner's son, learning Helix and ten-finger typing. The owner
(Michael, `rad@killian.email`) reviews the changes and plays them on staging.
Keep every text the child reads short, friendly and concrete.

## How every request is handled

1. **Document it as an issue first.** Title in plain words; body with the
   problem or request and, for a design, the agreed proposal.
2. **Fix it on a branch** (`feature/<topic>-<issue number>` or
   `fix/<topic>-<issue number>`) with a targeted change. No drive-by
   refactors; small related fixes found on the way are fine if the PR says so.
3. **Open a pull request** that says "Fixes #N", what changes, and how it was
   checked.
4. **State how confident you are**, and why (tests, browser check, what was not
   checked).
5. **Merge only when confident** and the `check` job is green. Merge commits
   (`merge_method=merge`), not squash.
6. Report back briefly: what came out, the confidence statement, and how the
   approach worked (the owner likes to learn which approach worked for complex
   things). Prefer honest reflection over reassurance.

Ask before building when a request is ambiguous and expensive to redo; for
designs, propose first (one or two options with trade-offs) and build once the
owner agrees. Clear, small requests: just do them.

Staging is GitHub Pages: <https://michi-zuri.github.io/helix-in-wonderland/>,
published by `.github/workflows/pages.yml` on every push to `main`.

Commits and PRs end with the attribution lines the session's own instructions
give (co-author trailer and session link).

### Commands used for the routine (GitHub, today)

```sh
gh api repos/michi-zuri/helix-in-wonderland/issues -f title="…" -F body=@issue.md --jq .number
git push -u origin <branch>
gh api repos/michi-zuri/helix-in-wonderland/pulls -f title="…" -f head=<branch> -f base=main -F body=@pr.md --jq .number
gh api repos/michi-zuri/helix-in-wonderland/commits/<sha>/check-runs --jq '[.check_runs[] | .status + ":" + (.conclusion // "-")]'
gh api -X PUT repos/michi-zuri/helix-in-wonderland/pulls/<n>/merge -f merge_method=merge
```

The local folder may be called `helix-wonderland`, but the repository is
`michi-zuri/helix-in-wonderland`; if a push fails with 403, check
`git remote get-url origin` first.

## Before saying something works

- `npm run check` (format, elm-review, all tests) and `npm run build`.
  `npx elm-review --fix-all-without-prompt` clears unused code after removals;
  `npm run format` fixes formatting.
- **Every new or changed key is pressed inside the real chapter**, not only in
  `Editor.Core`. (The Curiouser chapter once blocked `gg`/`ge` because its lock list
  wasn't updated, #51: the editor tests passed, the chapter didn't.) Chapters
  decide which commands are unlocked (`isUnlocked`), so test through the
  chapter's `update`.
- **Program tests for anything the shell does** (`tests/ProgramFlowTest.elm`,
  elm-program-test, #75): menus, starting chapters, results, the end card,
  saved progress and settings, fullscreen. They run the whole `App` with
  simulated keys ("keydown"), frames ("frame") and ports, so they replace
  clicking through the game in a browser. Press exactly the keys a player
  would: an extra Enter after a chapter goes on to the results, the next
  chapter or the menu. `App.update` returns `App.Command`s as data;
  `Main` only turns them into ports and subscriptions.
- **A Playwright screenshot only for visual changes** (layout, CSS, long
  lines, fullscreen), which program tests can't see: clipped goal lines (#65)
  and the cut-off title button (#69) were found that way. Playwright and
  Chromium are usually pre-installed (`require('playwright')`,
  `chromium.launch()`). Open `dist/site/index.html`, click "Unlock all
  chapters", choose a chapter (`j` × n, Enter), press Enter until
  `.overlay .card` appears, wait 600 ms, press `1`/`2`/`3`. Keys like `ö` need
  `document.dispatchEvent(new KeyboardEvent('keydown', { key }))`, not
  `keyboard.type`. Screenshots stay in the scratchpad; never commit them.
- **Elm packages without package.elm-lang.org**: if the sandbox can't reach
  it, `python3 tools/fetch_elm_packages.py <author/name>` and
  `python3 tools/make_elm_registry.py` fetch packages from GitHub (README).

## Design decisions to keep

**Every chapter**
- After the opening story, a difficulty card: Easy / Challenge / Mad
  (`Difficulty`, `Ui.DifficultyChoice`). The most tarts are 1 / 2 / 3; mistakes
  can cost tarts below that. Keys `1 2 3` or `j k` + Enter.
- The puzzle text follows the language setting until the difficulty is picked,
  then stays fixed.
- Instructions wait until they are relevant; during the story the panel is
  empty (`Chapter.Story.instructions`). Hints come **one at a time**, the one
  that helps right now.
- "Our best" on the results card is a route **we found and verified in a test**
  that plays it through the real chapter, using **only keys the game has taught
  by then**. Exact shortest-path solvers were tried and were far too slow (over
  8 minutes); a hand-tuned route plus a test is the approach.
- Hidden sentences: chapter 4's word sequences and chapter 6's odd words form
  hidden sentences. The game never points them out; keep them secret in the UI.
- No on-screen keyboard (removed in #53). Fullscreen mode (#57) shows only stage,
  panel and status line; Esc leaves browser fullscreen, so on entering it the
  player picks their own escape keys (like `;;`), which must come within 300 ms
  of each other (#59, #61). `Context.escapeKeys` carries them to chapters.

**Keys, by chapter** (what's taught where; don't use untaught keys in hints or
"Our best" routes)
1. Home row typing in one long fall: left hand finger by finger, then the
right hand, then `g h`, then both hands (#72). 2. `h j k l`, counts like `5j`.
3. Insert mode with the golden key: `i`, Esc, `c` (#73); typed letters are
home row only. 4. Typing words, `e`, `i`, space (#74 will add `o` and `.`).
5. `gh gl gg ge gj gk g.` (`gw` works but is deliberately not taught;
finishing chapter 5 with only `gw` jumps and no mistake brings out the
Cheshire Cat, an easter egg nothing announces).
6. `w`, `x`, `d`, `u`, counts like `3w`, `2x`, `gh`. 7. `a`, plus the Insert
mode from chapter 3; Challenge adds `w c` and `gl a`, Mad adds `x d` for a
line that doesn't belong.

Chapter numbers changed over time: up to #68 Drink Me was 3 and the
Caterpillar 6; #69–#70 briefly had a right-hand chapter first; since #73 the
order above holds. Read old issues with that in mind.

- **New typing letters come in batches of at most 5** on top of those learned
  before (#71).
- `e` and `b` are **locked** in chapters 6 and 7: the owner wants to teach them
  later, with more thought about why they're needed.

**Chapter specifics worth knowing**
- Chapter 2, mad hall: doors lead to random doors, except the way back always
  returns where you came from; only the pair just used stays linked (#29–#32).
  A door that would lead into the curtain's part of the hall steers away after a
  hidden 4-second clock, so a slow player can't reach the curtain that way; the
  countdown is deliberately not shown (#63).
- Chapter 1: twelve stages in one fall; a player who stays in the last stage
  (both hands) for a minute without reaching the ground goes round the whole
  left hand, the whole right hand and both hands again (#72).
  No panel: test players missed instructions there, so each stage opens with
  a dialog that pauses the fall, the first one saying to press a jar's letter
  as soon as it shows; the status line keeps the stage's keys and the depth
  (#81). An empty panel is hidden in every chapter (`.panel:empty`).
- A chapter opens if the one before has a tart, or if it or any later chapter
  has one, so reordering never locks away what a player reached
  (`Progress.isOpen`). Retired save names (`rabbit-hole-right-hand`) count for
  nothing.
- Chapter 3: the key is Insert mode. `i` puts it into a lock (INS, the key
  sticks out of the keyhole of the cursor's line), every key then works the
  lock, `h j k l` too; Esc pulls it out. Each gap or wrong letter is a lock;
  gaps between words are two spaces, a gap before punctuation one space. On
  mad the key slips out after 2 s without a key (#73).
- Chapter 4: typing from memory; a word hides after its first key and starts
  over after a pause over 300 ms between correct keys (#35).
- Chapter 5: lines without indents (#55); cheeky currants (`g.`) are shy and run
  off once the cursor shares their line, screen row or column (#47).
- Chapter 7: Mad is Alice's garbled "You are old, Father William" (#65). A
  smoke-ring twist was discussed and postponed.

## Conventions (in addition to README.md)

- German: Swiss spelling, `ss`, never `ß`. Both languages in `Texts.elm` always.
- Keyboard layouts: QWERTZ DE, QWERTZ CH, QWERTY, AZERTY. Keys are handled by
  the character they type.
- Saved-data keys never change (`Chapter.Id.toKey`, `Keyboard.Layout.toKey`).
- Story text is from the 1865 book or newly written; characters are drawn from
  the book, never from films.
- Time only through `Frame` messages, randomness only through seeds.

## Techniques that worked

- **Route harness for puzzles**: a throwaway test module that plays a route
  through the chapter and fails with a log of cursor, selection, keystrokes and
  current line after each step. Tune the route step by step, then turn it into
  a permanent test and put its keystroke count in `Texts`. Useful facts about
  this Helix model: after Esc the cursor sits on the last typed character;
  `j` onto a shorter line lands past its end, where `i` appends; `w` selects the
  word plus its trailing space, so `c` needs the space typed back.
- **Playthrough tests** (`tests/Playthrough.elm`: `start`, `send`, `sendAll`,
  `finishedWith`, `toastCount`) with `Frame` messages for time, and simulated
  players (patient, slow) for timing features.

## Next: moving from GitHub to a self-hosted Forgejo

The owner plans to move the repository to Forgejo on their own NixOS server,
keeping the same issue → PR → merge routine. The chat this was planned in
couldn't reach other hosts, so the work continues in a Claude Code cloud
session whose environment has:

- **Network access: Custom**, with the Forgejo domain listed, and the default
  package registries kept (npm is needed for Elm).
- **A network secret** for the Forgejo domain holding a Forgejo access token
  (header `Authorization`, prefix `token`), ideally from a bot account limited
  to this repository. The proxy adds it to requests, so the token never appears
  in the session.

Plan, each step as its own issue and PR where it changes the repo:

1. Check the connection: `git ls-remote https://<forgejo>/<owner>/helix-in-wonderland.git`
   and `curl https://<forgejo>/api/v1/version`.
2. The owner imports the GitHub repo in Forgejo ("New migration → GitHub", with
   issues, PRs, labels, milestones and releases) so #1–#66 come along.
3. Port `.github/workflows/pages.yml` to Forgejo Actions (`.forgejo/workflows/`):
   same `npm ci`, `npm run check`, `npm run build`; replace the GitHub Pages
   deploy with copying `dist/site` to a folder the server's web server serves
   (staging). The runner is the NixOS `services.gitea-actions-runner` with
   `pkgs.forgejo-runner`.
4. Make `src/Project.elm` and `tools/build.mjs` (which write the repository and
   "Suggest a change" / pixel-art proposal links into the page) work with a
   Forgejo URL, and update README.md and ART.md.
5. Switch `origin`, then replace the `gh` commands above with Forgejo's API
   (Gitea-compatible, `/api/v1`; verify against the server's `/api/swagger`):
   - issue: `POST /repos/{owner}/{repo}/issues` `{title, body}`
   - pull request: `POST /repos/{owner}/{repo}/pulls` `{title, body, head, base}`
   - CI status: `GET /repos/{owner}/{repo}/commits/{sha}/status`
   - merge: `POST /repos/{owner}/{repo}/pulls/{n}/merge` `{"Do": "merge"}`
   Then update the routine section of this file.
6. Optionally keep GitHub as a push mirror for the code.
