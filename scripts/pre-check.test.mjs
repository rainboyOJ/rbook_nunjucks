import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { validateCodeDirectory } from '../packages/rbook-core/dist/validation.js';
import { evaluatePreCheck } from '../packages/rbook-search/dist/preCheck.js';

function codeDirectory(t) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-code-inventory-'));
  fs.writeFileSync(path.join(directory, 'readme.md'), 'templates only\n');
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  return directory;
}

function page(id, path = `${id}.md`, extraFrontMatter = {}) {
  return {
    id: path,
    path,
    url: `/${path.replace(/\.md$/, '.html')}`,
    title: `Page ${id}`,
    visible: true,
    source: 'test',
    navTrail: [],
    headings: [],
    text: 'Test',
    excerpt: 'Test',
    frontMatter: {
      id,
      title: `Page ${id}`,
      description: 'Description',
      tags: ['test'],
      ...extraFrontMatter
    }
  };
}

test('the shared pre-check returns structured success data', () => {
  const result = evaluatePreCheck([page('valid-page')], []);
  assert.deepEqual(result, {
    errors: [],
    warnings: [],
    stats: { pages: 1, codes: 0, errors: 0, warnings: 0 },
    ok: true
  });
});

test('warnings do not fail the shared pre-check', () => {
  const warningPage = page('warning-page');
  delete warningPage.frontMatter.description;
  const result = evaluatePreCheck([warningPage], []);
  assert.equal(result.ok, true);
  assert.equal(result.errors.length, 0);
  assert.ok(result.warnings.some((item) => item.message.includes('description')));
});

test('duplicate IDs and unknown code references are errors', () => {
  const duplicateResult = evaluatePreCheck([
    page('duplicate-id', 'first.md'),
    page('duplicate-id', 'second.md')
  ], []);
  assert.equal(duplicateResult.ok, false);
  assert.ok(duplicateResult.errors.some((item) => item.message.includes("duplicate id 'duplicate-id'")));

  const referenceResult = evaluatePreCheck([
    page('reference-page', 'reference.md', { code_template: ['missing-code'] })
  ], []);
  assert.equal(referenceResult.ok, false);
  assert.ok(referenceResult.errors.some((item) => item.message.includes("'missing-code' is not registered")));
});

test('include-code paths are checked, including legacy fenced placeholders', () => {
  const missingPath = page('missing-code-path', 'missing-code-path.md');
  missingPath.sourceContent = '```cpp\n@include-code(./missing.cpp)\n```\n';
  const result = evaluatePreCheck([missingPath], []);
  assert.equal(result.ok, false);
  assert.ok(result.errors.some((item) => item.message.includes("@include-code './missing.cpp' failed")));
});

test('the shared pre-check does not modify process.exitCode', () => {
  const previousExitCode = process.exitCode;
  process.exitCode = undefined;
  try {
    const result = evaluatePreCheck([page(123, 'numeric.md')], []);
    assert.equal(result.ok, false);
    assert.equal(process.exitCode, undefined);
  } finally {
    process.exitCode = previousExitCode;
  }
});

test('the code directory accepts exactly the registered template files', (t) => {
  const directory = codeDirectory(t);
  fs.writeFileSync(path.join(directory, 'sample.cpp'), '// template\n');

  const errors = validateCodeDirectory([
    { id: 'sample', path: 'sample.cpp', description: 'Sample' }
  ], { codeDir: directory });
  assert.deepEqual(errors, []);
});

test('the code directory rejects duplicate, unregistered, and build artifact files', (t) => {
  const directory = codeDirectory(t);
  fs.writeFileSync(path.join(directory, 'sample.cpp'), '// template\n');
  fs.writeFileSync(path.join(directory, 'orphan.cpp'), '// not registered\n');
  fs.writeFileSync(path.join(directory, 'debug.out'), 'binary\n');
  fs.mkdirSync(path.join(directory, 'debug.dSYM'));

  const errors = validateCodeDirectory([
    { id: 'sample-a', path: 'sample.cpp', description: 'Sample A' },
    { id: 'sample-b', path: 'sample.cpp', description: 'Sample B' }
  ], { codeDir: directory });
  const messages = errors.map((item) => item.message);
  assert.ok(messages.some((message) => message.includes("duplicate code path 'sample.cpp'")));
  assert.ok(messages.some((message) => message.includes("unregistered code file 'orphan.cpp'")));
  assert.ok(messages.some((message) => message.includes("build artifact 'debug.out'")));
  assert.ok(messages.some((message) => message.includes("build artifact 'debug.dSYM'")));
});

test('registered code paths must remain inside the code directory and be regular files', (t) => {
  const directory = codeDirectory(t);
  fs.mkdirSync(path.join(directory, 'nested.cpp'));
  const errors = validateCodeDirectory([
    { id: 'outside', path: '../outside.cpp', description: 'Outside' },
    { id: 'directory', path: 'nested.cpp', description: 'Directory' }
  ], { codeDir: directory });
  const messages = errors.map((item) => item.message);
  assert.ok(messages.some((message) => message.includes('outside the code directory')));
  assert.ok(messages.some((message) => message.includes("'nested.cpp' for id 'directory' must be a regular file")));
});
