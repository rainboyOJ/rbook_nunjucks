import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import test from 'node:test';
import {
  parsePublicId,
  requireCodeId,
  requirePageId,
  validateCodes,
  validatePages
} from '../packages/rbook-core/dist/validation.js';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const buildIndexUrl = pathToFileURL(
  path.join(rootDir, 'packages/rbook-search/dist/buildIndex.js')
).href;
const searchIndexUrl = pathToFileURL(
  path.join(rootDir, 'packages/rbook-search/dist/searchIndex.js')
).href;

function page(id, pathName = 'page.md') {
  return {
    path: pathName,
    title: 'Test page',
    frontMatter: {
      id,
      title: 'Test page',
      description: 'Test description',
      tags: ['test']
    }
  };
}

function errorMessages(items) {
  return items.filter((item) => item.level === 'ERROR').map((item) => item.message);
}

function createContentFixture(t, idSource) {
  const fixtureDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-id-contract-'));
  const contentDir = path.join(fixtureDir, 'book');
  const runtimeDir = path.join(fixtureDir, 'runtime');
  fs.mkdirSync(path.join(contentDir, 'pages'), { recursive: true });
  fs.mkdirSync(path.join(contentDir, 'code'), { recursive: true });
  fs.writeFileSync(
    path.join(contentDir, 'book.yaml'),
    'title: Test\nchapters: []\nglob: []\n'
  );
  fs.writeFileSync(path.join(contentDir, 'code.yaml'), 'codes: []\n');
  fs.writeFileSync(
    path.join(contentDir, 'pages/index.md'),
    `---\nid: ${idSource}\ntitle: Numeric string ID\ndescription: Test\ntags: [test]\n---\n# Test\n`
  );
  t.after(() => fs.rmSync(fixtureDir, { recursive: true, force: true }));
  return { contentDir, runtimeDir };
}

function runIndexChild(contentDir, runtimeDir, source) {
  return spawnSync(process.execPath, ['--input-type=module', '-e', source], {
    cwd: rootDir,
    env: {
      ...process.env,
      RBOOK_CONTENT_DIR: contentDir,
      RBOOK_CODE_DIR: path.join(contentDir, 'code'),
      RBOOK_RUNTIME_DIR: runtimeDir
    },
    encoding: 'utf8'
  });
}

test('public IDs reject implicit conversion and accept explicit numeric strings', () => {
  assert.deepEqual(parsePublicId(123), { ok: false, error: 'type' });
  assert.deepEqual(parsePublicId(''), { ok: false, error: 'empty' });
  assert.deepEqual(parsePublicId('   '), { ok: false, error: 'empty' });
  assert.deepEqual(parsePublicId('UPPER'), { ok: false, error: 'format' });
  assert.deepEqual(parsePublicId('bad_id'), { ok: false, error: 'format' });
  assert.deepEqual(parsePublicId('123'), { ok: true, id: '123' });
});

test('page validation reports numeric IDs with their source path', () => {
  const errors = validatePages([page(123, 'numeric.md')]);
  assert.ok(errors.some((item) =>
    item.level === 'ERROR'
      && item.filePath === 'numeric.md'
      && item.message.includes('文章 ID 必须是字符串')
  ));
  assert.throws(() => requirePageId(page(123, 'numeric.md')), /numeric\.md.*文章 ID 必须是字符串/);
});

test('empty, whitespace, duplicate, uppercase and punctuation page IDs fail', () => {
  const cases = [
    page('', 'empty.md'),
    page('   ', 'whitespace.md'),
    page('UPPER', 'uppercase.md'),
    page('bad_id', 'punctuation.md'),
    page('duplicate', 'first.md'),
    page('duplicate', 'second.md')
  ];
  const messages = errorMessages(validatePages(cases));
  assert.ok(messages.some((message) => message.includes("missing required field 'id'")));
  assert.ok(messages.some((message) => message.includes("page id 'UPPER'")));
  assert.ok(messages.some((message) => message.includes("page id 'bad_id'")));
  assert.ok(messages.some((message) => message.includes("duplicate id 'duplicate'")));
});

test('code IDs use the same runtime type rule', () => {
  const code = {
    id: 123,
    path: 'tree/dsu_on_tree_color_count.cpp',
    description: 'Test code'
  };
  const errors = validateCodes([code]);
  assert.ok(errors.some((item) => item.message.includes('代码 ID 必须是字符串')));
  assert.throws(() => requireCodeId(code), /代码 ID 必须是字符串/);
});

test('a quoted numeric ID is indexed and can be queried by the same ID', (t) => {
  const { contentDir, runtimeDir } = createContentFixture(t, '"123"');
  const source = `
    const { buildSearchIndex } = await import(${JSON.stringify(buildIndexUrl)});
    const payload = buildSearchIndex();
    const { getPageById } = await import(${JSON.stringify(searchIndexUrl)});
    const page = getPageById('123');
    console.log(JSON.stringify({ indexedId: payload.pages[0].id, queriedId: page?.id }));
  `;
  const result = runIndexChild(contentDir, runtimeDir, source);
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
  const payload = JSON.parse(result.stdout.trim());
  assert.deepEqual(payload, { indexedId: '123', queriedId: '123' });
});

test('a numeric YAML ID aborts indexing before an index file is written', (t) => {
  const { contentDir, runtimeDir } = createContentFixture(t, '123');
  const source = `
    const { buildSearchIndex } = await import(${JSON.stringify(buildIndexUrl)});
    try {
      buildSearchIndex();
      process.exit(2);
    } catch (error) {
      console.log(error instanceof Error ? error.message : String(error));
    }
  `;
  const result = runIndexChild(contentDir, runtimeDir, source);
  assert.equal(result.status, 0, `${result.stdout}\n${result.stderr}`);
  assert.match(result.stdout, /index\.md: 文章 ID 必须是字符串/);
  assert.equal(fs.existsSync(path.join(runtimeDir, '.search/index.json')), false);
});
