import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import test from 'node:test';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const widgetDir = path.join(root, 'site/widgets/code_template_filter');
const sourcePath = path.join(widgetDir, 'index.html');
const buildRuntimePath = path.join(root, 'packages/rbook-server/dist/buildRuntime.js');
const widgetApps = [
  ['code_template_filter', 'code_template'],
  ['explore', 'explore'],
  ['article_inspector', 'article_inspector'],
  ['tags', 'tags'],
  ['relations', 'relations'],
  ['practice', 'practice'],
  ['diagnostics', 'diagnostics']
];

function createTempDir(t) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-code-runtime-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  return directory;
}

function runStaticCopy(t) {
  const runtimeDir = createTempDir(t);
  const script = `import { buildCodeTemplateApp } from ${JSON.stringify(pathToFileURL(buildRuntimePath).href)}; buildCodeTemplateApp();`;
  const result = spawnSync(process.execPath, ['--input-type=module', '--eval', script], {
    cwd: root,
    env: {...process.env, RBOOK_RUNTIME_DIR: runtimeDir},
    encoding: 'utf8'
  });
  return {
    ...result,
    output: `${result.stdout || ''}\n${result.stderr || ''}`,
    outputPath: path.join(runtimeDir, 'dist/code_template/index.html')
  };
}

test('code template widget is a single API-backed HTML file', () => {
  assert.deepEqual(fs.readdirSync(widgetDir).sort(), ['index.html']);
  const html = fs.readFileSync(sourcePath, 'utf8');
  assert.match(html, /requestJson\('\/api\/codes'\)/);
  assert.match(html, /includeContent=true/);
  assert.match(html, /Prism/);
  assert.doesNotMatch(html, /vue-toastification|createApp|template_array/);
});

test('runtime build copies the single widget file without Vite', (t) => {
  const result = runStaticCopy(t);
  assert.equal(result.status, 0, result.output);
  assert.equal(fs.readFileSync(result.outputPath, 'utf8'), fs.readFileSync(sourcePath, 'utf8'));
});

test('all widget sources are single HTML files', () => {
  for (const [source] of widgetApps) {
    const directory = path.join(root, 'site/widgets', source);
    assert.deepEqual(fs.readdirSync(directory).sort(), ['index.html'], source);
    const html = fs.readFileSync(path.join(directory, 'index.html'), 'utf8');
    assert.match(html, /<!doctype html>/i, source);
    assert.doesNotMatch(html, /https?:\/\/127\.0\.0\.1|https?:\/\/localhost/, source);
  }
});

test('runtime build copies every static widget to its public directory', (t) => {
  const runtimeDir = createTempDir(t);
  const script = `import { buildStaticWidgetApps } from ${JSON.stringify(pathToFileURL(buildRuntimePath).href)}; buildStaticWidgetApps();`;
  const result = spawnSync(process.execPath, ['--input-type=module', '--eval', script], {
    cwd: root,
    env: {...process.env, RBOOK_RUNTIME_DIR: runtimeDir},
    encoding: 'utf8'
  });
  assert.equal(result.status, 0, `${result.stdout || ''}\n${result.stderr || ''}`);
  for (const [source, target] of widgetApps) {
    const sourceFile = path.join(root, 'site/widgets', source, 'index.html');
    const targetFile = path.join(runtimeDir, 'dist', target, 'index.html');
    assert.equal(fs.readFileSync(targetFile, 'utf8'), fs.readFileSync(sourceFile, 'utf8'), target);
  }
});
