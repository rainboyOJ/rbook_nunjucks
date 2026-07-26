import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { loadCodeConfig } from '../packages/rbook-core/dist/validation.js';

function createTempDir(t) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-code-config-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  return directory;
}

test('loads a valid code configuration from an explicit path', (t) => {
  const directory = createTempDir(t);
  const codeYamlPath = path.join(directory, 'code.yaml');
  fs.writeFileSync(codeYamlPath, `codes:
  - id: sample
    path: sample.cpp
    description: Sample template
`);

  assert.deepEqual(loadCodeConfig({ codeYamlPath, strict: true }), {
    codes: [{ id: 'sample', path: 'sample.cpp', description: 'Sample template' }]
  });
});

test('strict mode rejects a missing configuration file', (t) => {
  const directory = createTempDir(t);
  const codeYamlPath = path.join(directory, 'missing.yaml');

  assert.throws(
    () => loadCodeConfig({ codeYamlPath, strict: true }),
    /file does not exist/
  );
  assert.deepEqual(loadCodeConfig({ codeYamlPath }), { codes: [] });
});

test('rejects invalid YAML with the source path in the error', (t) => {
  const directory = createTempDir(t);
  const codeYamlPath = path.join(directory, 'code.yaml');
  fs.writeFileSync(codeYamlPath, 'codes: [invalid\n');

  assert.throws(
    () => loadCodeConfig({ codeYamlPath, strict: true }),
    (error) => error.message.includes(codeYamlPath) && error.message.includes('failed to load')
  );
});

test('strict mode rejects an invalid codes shape', (t) => {
  const directory = createTempDir(t);
  const codeYamlPath = path.join(directory, 'code.yaml');
  fs.writeFileSync(codeYamlPath, 'codes: {}\n');

  assert.throws(
    () => loadCodeConfig({ codeYamlPath, strict: true }),
    /field 'codes' must be an array/
  );
  assert.deepEqual(loadCodeConfig({ codeYamlPath }), { codes: [] });
});
