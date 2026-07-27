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
