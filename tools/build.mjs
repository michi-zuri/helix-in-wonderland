// Build the game into two outputs from public/index.html:
//   dist/helix-wonderland.html  a page fragment for claude.ai artifacts (they add the <html> wrapper)
//   dist/site/                  a complete static website for Netlify or any other host
import { execFileSync } from 'node:child_process';
import { copyFileSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';

const compiled = 'build/elm.js';
execFileSync('npx', ['elm', 'make', 'src/Main.elm', '--optimize', `--output=${compiled}`], { stdio: 'inherit' });

const script = readFileSync(compiled, 'utf8').replaceAll('</script', '<\\/script');
const page = readFileSync('public/index.html', 'utf8').replace('<!-- ELM -->', () => `<script>\n${script}\n</script>`);

mkdirSync('dist/site', { recursive: true });
writeFileSync('dist/helix-wonderland.html', page);

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

for (const file of ['dist/helix-wonderland.html', 'dist/site/index.html']) {
  console.log(`${file}  (${Math.round(statSync(file).size / 1024)} KB)`);
}
