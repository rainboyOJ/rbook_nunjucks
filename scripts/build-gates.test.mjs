import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import test from 'node:test';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const buildRuntimeUrl = pathToFileURL(
  path.join(rootDir, 'packages/rbook-server/dist/buildRuntime.js')
).href;
const entryScripts = [
  'build:packages',
  'build',
  'build:all',
  'build:index',
  'build:runtime',
  'dev'
];

function markdown(id, codeTemplate = null, includeWarnings = false) {
  const reference = codeTemplate ? `code_template: [${codeTemplate}]\n` : '';
  const metadata = includeWarnings ? '' : 'description: Test page\ntags: [test]\n';
  return `---\nid: ${id}\ntitle: Test page\n${metadata}${reference}---\n# Test\n`;
}

function createFixture(t, scenario) {
  const fixtureDir = fs.mkdtempSync(path.join(os.tmpdir(), `rbook-build-gate-${scenario}-`));
  const appDir = path.join(fixtureDir, 'site');
  const contentDir = path.join(fixtureDir, 'book');
  fs.mkdirSync(path.join(appDir, 'dist'), { recursive: true });
  fs.mkdirSync(path.join(contentDir, 'pages'), { recursive: true });
  fs.mkdirSync(path.join(contentDir, 'code'), { recursive: true });
  fs.writeFileSync(path.join(contentDir, 'book.yaml'), 'title: Test\nchapters: []\nglob: []\n');
  fs.writeFileSync(path.join(contentDir, 'code/known.cpp'), 'int main() {}\n');
  fs.writeFileSync(path.join(contentDir, 'code.yaml'), `codes:
  - id: known-code
    path: known.cpp
    description: Known code
    tags: [test]
`);

  if (scenario === 'duplicate') {
    fs.writeFileSync(path.join(contentDir, 'pages/index.md'), markdown('index-page'));
    fs.writeFileSync(path.join(contentDir, 'pages/first.md'), markdown('duplicate-id'));
    fs.writeFileSync(path.join(contentDir, 'pages/second.md'), markdown('duplicate-id'));
  } else if (scenario === 'unknown-reference') {
    fs.writeFileSync(path.join(contentDir, 'pages/index.md'), markdown('index-page', 'missing-code'));
  } else {
    fs.writeFileSync(path.join(contentDir, 'pages/index.md'), markdown('index-page', null, true));
  }

  t.after(() => fs.rmSync(fixtureDir, { recursive: true, force: true }));
  return { fixtureDir, appDir, contentDir };
}

function environment(fixture, runtimeDir) {
  return {
    ...process.env,
    FORCE_COLOR: '0',
    NO_COLOR: '1',
    RBOOK_APP_DIR: fixture.appDir,
    RBOOK_CONTENT_DIR: fixture.contentDir,
    RBOOK_CODE_DIR: path.join(fixture.contentDir, 'code'),
    RBOOK_RUNTIME_DIR: runtimeDir,
    HOST: '127.0.0.1',
    PORT: '0'
  };
}

function prepareSentinels(fixture, runtimeDir) {
  const runtimeSentinel = path.join(runtimeDir, 'dist/runtime-sentinel.txt');
  const appSentinel = path.join(fixture.appDir, 'dist/app-sentinel.txt');
  const searchIndexPath = path.join(runtimeDir, '.search/index.json');
  fs.mkdirSync(path.dirname(runtimeSentinel), { recursive: true });
  fs.writeFileSync(runtimeSentinel, 'keep-runtime\n');
  fs.writeFileSync(appSentinel, 'keep-app\n');
  return { runtimeSentinel, appSentinel, searchIndexPath };
}

function runNpmScript(script, env) {
  const result = spawnSync('npm', ['run', '--silent', script], {
    cwd: rootDir,
    env,
    encoding: 'utf8',
    timeout: 60_000,
    maxBuffer: 20 * 1024 * 1024
  });
  return { ...result, output: `${result.stdout || ''}\n${result.stderr || ''}` };
}

function assertSentinels(sentinels, label) {
  assert.equal(fs.readFileSync(sentinels.runtimeSentinel, 'utf8'), 'keep-runtime\n', `${label} changed runtime output`);
  assert.equal(fs.readFileSync(sentinels.appSentinel, 'utf8'), 'keep-app\n', `${label} changed app output`);
  assert.equal(fs.existsSync(sentinels.searchIndexPath), false, `${label} wrote a search index`);
}

for (const scenario of [
  { name: 'duplicate', pattern: /duplicate id 'duplicate-id'/ },
  { name: 'unknown-reference', pattern: /'missing-code' is not registered/ }
]) {
  test(`all public build entries reject ${scenario.name}`, (t) => {
    const fixture = createFixture(t, scenario.name);

    for (const script of entryScripts) {
      const runtimeDir = path.join(fixture.fixtureDir, `runtime-${script.replaceAll(':', '-')}`);
      const sentinels = prepareSentinels(fixture, runtimeDir);
      const result = runNpmScript(script, environment(fixture, runtimeDir));
      assert.notEqual(result.status, 0, `${script} unexpectedly succeeded\n${result.output}`);
      assert.match(result.output, scenario.pattern, `${script} did not report the expected validation error`);
      assertSentinels(sentinels, script);
    }
  });
}

test('warnings keep the pre-check CLI successful', (t) => {
  const fixture = createFixture(t, 'warning');
  const runtimeDir = path.join(fixture.fixtureDir, 'runtime-warning');
  const result = runNpmScript('pre-check:compiled', environment(fixture, runtimeDir));
  assert.equal(result.status, 0, result.output);
  assert.match(result.output, /warnings=[1-9][0-9]*/);
  assert.match(result.output, /errors=0/);
});

test('direct buildRuntime and dev startup validate before clearing runtime output', (t) => {
  const fixture = createFixture(t, 'duplicate');

  for (const entry of [
    {
      label: 'buildRuntime()',
      command: [
        process.execPath,
        '--input-type=module',
        '-e',
        `const { buildRuntime } = await import(${JSON.stringify(buildRuntimeUrl)}); buildRuntime();`
      ]
    },
    {
      label: 'dev.js',
      command: [process.execPath, path.join(rootDir, 'packages/rbook-server/dist/dev.js')]
    }
  ]) {
    const runtimeDir = path.join(fixture.fixtureDir, `runtime-direct-${entry.label.replace(/[^a-z]/gi, '')}`);
    const sentinels = prepareSentinels(fixture, runtimeDir);
    const result = spawnSync(entry.command[0], entry.command.slice(1), {
      cwd: rootDir,
      env: environment(fixture, runtimeDir),
      encoding: 'utf8',
      timeout: 30_000
    });
    const output = `${result.stdout || ''}\n${result.stderr || ''}`;
    assert.notEqual(result.status, 0, `${entry.label} unexpectedly succeeded\n${output}`);
    assert.match(output, /duplicate id 'duplicate-id'/);
    assertSentinels(sentinels, entry.label);
  }
});
