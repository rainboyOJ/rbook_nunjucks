import assert from 'node:assert/strict';
import test from 'node:test';
import { decideBuildGates, touchesBuildFlow } from './build-gate-scope.mjs';

test('build flow files trigger the gates', () => {
  for (const file of [
    'package.json',
    'package-lock.json',
    'site/build.sh',
    'packages/rbook-search/src/preCheck.ts',
    'packages/rbook-core/src/index.ts',
    'packages/rbook-server/tsconfig.json',
    'tsconfig.json',
    '.github/workflows/ci.yml'
  ]) {
    assert.equal(touchesBuildFlow(file), true, `${file} should trigger build-gates`);
  }
});

test('content, template and doc changes do not trigger the gates', () => {
  for (const file of [
    'book/code/graph/diff_constraints.py',
    'book/pages/graph/diff-constraints/code/main.py',
    'book/pages/graph/diff-constraints/index.md',
    'book/book.yaml',
    'book/code.yaml',
    'scripts/pre-check.test.mjs',
    'README.md'
  ]) {
    assert.equal(touchesBuildFlow(file), false, `${file} should not trigger build-gates`);
  }
});

test('a content-only change skips the gates and explains why', () => {
  const reason = decideBuildGates({
    changedFiles: new Set(['book/pages/graph/diff-constraints/index.md']),
    env: {}
  });
  assert.match(reason, /跳过 build-gates/);
  assert.match(reason, /RBOOK_FORCE_BUILD_GATES=1/);
});

test('a build flow change runs the gates', () => {
  const reason = decideBuildGates({
    changedFiles: new Set(['book/pages/x/index.md', 'packages/rbook-search/src/preCheck.ts']),
    env: {}
  });
  assert.equal(reason, null);
});

test('force flag and CI override the scope check', () => {
  const contentOnly = new Set(['book/pages/x/index.md']);
  assert.equal(decideBuildGates({ changedFiles: contentOnly, env: { RBOOK_FORCE_BUILD_GATES: '1' } }), null);
  assert.equal(decideBuildGates({ changedFiles: contentOnly, env: { CI: 'true' } }), null);
});

test('an unknown change scope runs the gates instead of skipping', () => {
  assert.equal(decideBuildGates({ changedFiles: null, env: {} }), null);
});
