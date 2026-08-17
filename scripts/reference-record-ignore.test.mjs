import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath, pathToFileURL } from 'node:url';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const collectPagesUrl = pathToFileURL(
  path.join(rootDir, 'packages/rbook-search/dist/collectPages.js')
).href;

function createContentFixture(t) {
  const fixtureDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-reference-record-'));
  const contentDir = path.join(fixtureDir, 'book');
  const pagesDir = path.join(contentDir, 'pages');
  const articleDir = path.join(pagesDir, 'topic');
  const recordDir = path.join(articleDir, 'reference_record');

  fs.mkdirSync(recordDir, { recursive: true });
  fs.writeFileSync(path.join(contentDir, 'book.yaml'), 'title: Test\nchapters: []\nglob: []\n');
  fs.writeFileSync(path.join(pagesDir, 'index.md'), '# Index\n');
  fs.writeFileSync(path.join(articleDir, 'index.md'), '# Topic\n');
  fs.writeFileSync(path.join(recordDir, '2026-08-17-1200-topic.md'), '# Local research\n');

  t.after(() => fs.rmSync(fixtureDir, { recursive: true, force: true }));
  return contentDir;
}

test('reference_record Markdown is excluded from the collected page catalog', (t) => {
  const contentDir = createContentFixture(t);
  const program = `
    const { collectPages } = await import(${JSON.stringify(collectPagesUrl)});
    process.stdout.write(JSON.stringify(collectPages().pages.map((page) => page.path)));
  `;
  const result = spawnSync(process.execPath, ['--input-type=module', '-e', program], {
    cwd: rootDir,
    env: { ...process.env, RBOOK_CONTENT_DIR: contentDir },
    encoding: 'utf8'
  });

  assert.equal(result.status, 0, result.stderr);
  const paths = JSON.parse(result.stdout);
  assert.ok(paths.includes('index.md'));
  assert.ok(paths.includes('topic/index.md'));
  assert.ok(paths.every((pagePath) => !pagePath.includes('/reference_record/')));
});

test('article reference records are ignored by Git', () => {
  const result = spawnSync(
    'git',
    ['check-ignore', '--quiet', '--no-index', 'book/pages/topic/reference_record/record.md'],
    { cwd: rootDir, encoding: 'utf8' }
  );
  assert.equal(result.status, 0, result.stderr);
});
