import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import test from 'node:test';

const rootDir = path.resolve(import.meta.dirname, '..');

test('production server serves SPA fallback for unknown paths', async (t) => {
  const fixtureDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-serve-fallback-'));
  const runtimeDir = path.join(fixtureDir, 'runtime');
  const distDir = path.join(runtimeDir, 'dist');

  fs.mkdirSync(distDir, { recursive: true });
  fs.writeFileSync(path.join(distDir, 'index.html'), '<h1>homepage</h1>');
  fs.writeFileSync(path.join(distDir, 'real.txt'), 'real file');
  t.after(() => fs.rmSync(fixtureDir, { recursive: true, force: true }));

  process.env.RBOOK_RUNTIME_DIR = runtimeDir;

  const { createApp } = await import(pathToFileURL(
    path.join(rootDir, 'packages/rbook-server/dist/app.js')
  ));
  const app = await createApp({ logger: false });

  t.after(async () => app.close());

  for (const url of ['/data-structure/BIT', '/data-structure/BIT/', '/nope']) {
    const response = await app.inject({ method: 'GET', url });
    assert.equal(response.statusCode, 200, url);
    assert.match(response.body, /<h1>homepage<\/h1>/, url);
  }

  const realFile = await app.inject({ method: 'GET', url: '/real.txt' });
  assert.equal(realFile.statusCode, 200);
  assert.equal(realFile.body, 'real file');

  const api = await app.inject({ method: 'GET', url: '/api/does-not-exist' });
  assert.equal(api.statusCode, 404);
});
