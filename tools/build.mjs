// Build the game into two outputs from public/index.html:
//   dist/helix-wonderland.html  a page fragment for claude.ai artifacts (they add the <html> wrapper)
//   dist/site/                  a complete static website for Netlify or any other host,
//                               including the pixel editor (dist/site/pixel-editor.html)
import { execFileSync } from 'node:child_process';
import { copyFileSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';

// Which GitHub repository this build belongs to, so proposal links open
// issues there: GitHub Actions says so in GITHUB_REPOSITORY (forks too);
// locally, the git remote tells. Without either, the game falls back to the
// original repository (src/Project.elm).
function repository() {
  if (process.env.GITHUB_REPOSITORY) return process.env.GITHUB_REPOSITORY;
  try {
    const url = execFileSync('git', ['config', '--get', 'remote.origin.url'], { encoding: 'utf8' }).trim();
    const match = url.match(/github\.com[:/]([^/]+\/[^/]+?)(\.git)?$/);
    if (match) return match[1];
  } catch (error) {}
  return null;
}
const repo = repository();
const config = (extra = {}) =>
  `<script>window.HELIX_REPOSITORY = ${JSON.stringify(repo)};` +
  Object.entries(extra).map(([key, value]) => ` window.${key} = ${JSON.stringify(value)};`).join('') +
  `</script>`;

// The claude.ai copy can't find the pixel editor next to itself, so it links
// to the repository's default GitHub Pages address instead.
function pagesAddress(repo) {
  if (!repo) return 'https://michi-zuri.github.io/helix-in-wonderland/';
  const [owner, name] = repo.split('/');
  const host = `${owner.toLowerCase()}.github.io`;
  return name.toLowerCase() === host ? `https://${host}/` : `https://${host}/${name}/`;
}

const compiled = 'build/elm.js';
execFileSync('npx', ['elm', 'make', 'src/Main.elm', '--optimize', `--output=${compiled}`], { stdio: 'inherit' });

const script = readFileSync(compiled, 'utf8').replaceAll('</script', '<\\/script');
const template = readFileSync('public/index.html', 'utf8').replace('<!-- ELM -->', () => `<script>\n${script}\n</script>`);
const page = template.replace('<!-- CONFIG -->', () => config());

mkdirSync('dist/site', { recursive: true });
writeFileSync('dist/helix-wonderland.html', template.replace('<!-- CONFIG -->', () => config({ HELIX_SITE: pagesAddress(repo) })));

// The standalone site: everything before the app's mount point goes into <head>.
const split = page.indexOf('<div id="app">');
const head = page.slice(0, split).replace(/<meta name="claude:[^>]*>\n?/g, '');
const body = page.slice(split);
writeFileSync('dist/site/index.html', `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<meta name="description" content="Learn the Helix editor and ten-finger typing in Alice's Wonderland.">
<link rel="icon" href="favicon.svg" type="image/svg+xml">
<style>:root{padding-top:env(safe-area-inset-top,0px);padding-bottom:env(safe-area-inset-bottom,0px)}body{margin:0}[hidden]{display:none!important}</style>
${head}</head>
<body>
${body}</body>
</html>
`);
copyFileSync('public/favicon.svg', 'dist/site/favicon.svg');
copyFileSync('public/fonts/DepartureMono-LICENSE.txt', 'dist/site/font-license.txt');

console.log(`links: issues go to ${repo ?? '(original repository)'}`);
for (const file of ['dist/helix-wonderland.html', 'dist/site/index.html']) {
  console.log(`${file}  (${Math.round(statSync(file).size / 1024)} KB)`);
}

// The pixel editor for art proposals: a separate small Elm program.
const editorCompiled = 'build/pixel-editor.js';
execFileSync('npx', ['elm', 'make', 'src/PixelEditor.elm', '--optimize', `--output=${editorCompiled}`], { stdio: 'inherit' });
const editorScript = readFileSync(editorCompiled, 'utf8').replaceAll('</script', '<\\/script');
writeFileSync('dist/site/pixel-editor.html',
  readFileSync('public/pixel-editor.html', 'utf8')
    .replace('<!-- ELM -->', () => `<script>\n${editorScript}\n</script>`)
    .replace('<!-- CONFIG -->', () => config()));
console.log(`dist/site/pixel-editor.html  (${Math.round(statSync('dist/site/pixel-editor.html').size / 1024)} KB)`);
