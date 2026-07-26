import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const viteBin = path.join(root, 'node_modules/vite/bin/vite.js');
const viteConfig = path.join(root, 'site/widgets/code_template_filter/vite.config.ts');

function createTempDir(t, prefix) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), prefix));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  return directory;
}

function createContentFixture(t, codeYaml) {
  const contentDir = createTempDir(t, 'rbook-code-content-');
  fs.mkdirSync(path.join(contentDir, 'pages'), { recursive: true });
  fs.mkdirSync(path.join(contentDir, 'code'), { recursive: true });
  fs.writeFileSync(path.join(contentDir, 'book.yaml'), 'title: Test\nchapters: []\nglob: []\n');
  if (codeYaml !== null) {
    fs.writeFileSync(path.join(contentDir, 'code.yaml'), codeYaml);
  }
  return contentDir;
}

function runViteBuild(t, contentDir) {
  const runtimeDir = createTempDir(t, 'rbook-code-runtime-');
  const result = spawnSync(
    process.execPath,
    [viteBin, 'build', '--config', viteConfig, '--base', '/code_template/'],
    {
      cwd: root,
      env: {
        ...process.env,
        RBOOK_CONTENT_DIR: contentDir,
        RBOOK_CODE_DIR: path.join(contentDir, 'code'),
        RBOOK_RUNTIME_DIR: runtimeDir
      },
      encoding: 'utf8'
    }
  );
  return {
    ...result,
    output: `${result.stdout || ''}\n${result.stderr || ''}`,
    outputDir: path.join(runtimeDir, 'dist/code_template')
  };
}

function readJavaScriptBundles(directory) {
  const contents = [];
  const visit = (current) => {
    for (const entry of fs.readdirSync(current, { withFileTypes: true })) {
      const fullPath = path.join(current, entry.name);
      if (entry.isDirectory()) visit(fullPath);
      else if (entry.isFile() && entry.name.endsWith('.js')) {
        contents.push(fs.readFileSync(fullPath, 'utf8'));
      }
    }
  };
  visit(directory);
  return contents.join('\n');
}

test('real widget build embeds templates from book/code.yaml', (t) => {
  const result = runViteBuild(t, path.join(root, 'book'));
  assert.equal(result.status, 0, result.output);

  const bundles = readJavaScriptBundles(result.outputDir);
  assert.match(bundles, /dsu-on-tree-color-count/);
  assert.match(bundles, /tricks-fraction-class/);
  assert.equal(
    fs.existsSync(path.join(result.outputDir, 'code/tree/dsu_on_tree_color_count.cpp')),
    true
  );
});

test('widget build rejects a missing code.yaml', (t) => {
  const contentDir = createContentFixture(t, null);
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /invalid code config[\s\S]*file does not exist/);
});

test('widget build rejects invalid code.yaml', (t) => {
  const contentDir = createContentFixture(t, 'codes: [invalid\n');
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /failed to load code config/);
});

test('widget build rejects a missing template file', (t) => {
  const contentDir = createContentFixture(t, `codes:
  - id: missing-template
    path: missing.cpp
    description: Missing template
`);
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /code file 'missing\.cpp'[\s\S]*does not exist on disk/);
});

test('widget build rejects a directory used as a template file', (t) => {
  const contentDir = createContentFixture(t, `codes:
  - id: directory-template
    path: nested
    description: Directory template
`);
  fs.mkdirSync(path.join(contentDir, 'code/nested'));
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /code template file does not exist:[\s\S]*code\/nested/);
});

test('widget build rejects a template path outside book/code', (t) => {
  const contentDir = createContentFixture(t, `codes:
  - id: outside-template
    path: ../outside.cpp
    description: Outside template
`);
  fs.writeFileSync(path.join(contentDir, 'outside.cpp'), 'int main() {}\n');
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /code template path escapes book\/code/);
});

test('widget build rejects a symlink that resolves outside book/code', (t) => {
  const contentDir = createContentFixture(t, `codes:
  - id: symlink-template
    path: linked.cpp
    description: Symlink template
`);
  const outsidePath = path.join(contentDir, 'outside.cpp');
  fs.writeFileSync(outsidePath, 'int main() {}\n');
  fs.symlinkSync(outsidePath, path.join(contentDir, 'code/linked.cpp'));
  const result = runViteBuild(t, contentDir);

  assert.notEqual(result.status, 0, result.output);
  assert.match(result.output, /code template path resolves outside book\/code/);
});
