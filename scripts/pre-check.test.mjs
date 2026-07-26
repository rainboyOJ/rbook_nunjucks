import assert from 'node:assert/strict';
import test from 'node:test';
import { evaluatePreCheck } from '../packages/rbook-search/dist/preCheck.js';

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
