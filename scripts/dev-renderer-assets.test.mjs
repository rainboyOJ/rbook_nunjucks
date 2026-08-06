import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import test from 'node:test';

const rootDir = path.resolve(import.meta.dirname, '..');

test('development renderer serves article image assets', async (t) => {
  const fixtureDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-dev-assets-'));
  const contentDir = path.join(fixtureDir, 'book');
  const runtimeDir = path.join(fixtureDir, 'runtime');
  const imagePaths = [
    'pages/data_structure/BIT/BITn7.png',
    'pages/data_structure/BIT/images/BITn9.png'
  ];

  for (const imagePath of imagePaths) {
    const fullPath = path.join(contentDir, imagePath);
    fs.mkdirSync(path.dirname(fullPath), { recursive: true });
    fs.writeFileSync(fullPath, 'test image');
  }
  t.after(() => fs.rmSync(fixtureDir, { recursive: true, force: true }));

  process.env.RBOOK_CONTENT_DIR = contentDir;
  process.env.RBOOK_RUNTIME_DIR = runtimeDir;

  const { default: DevRenderer } = await import(pathToFileURL(
    path.join(rootDir, 'packages/rbook-server/dist/devRenderer.js')
  ));
  const renderer = Object.create(DevRenderer.prototype);
  renderer.book = { config: {}, renderMenu: () => '' };
  for (const imageUrl of [
    '/data_structure/BIT/BITn7.png',
    '/data_structure/BIT/images/BITn9.png'
  ]) {
    const response = renderer.render(imageUrl);

    assert.equal(response?.statusCode, 200, imageUrl);
    assert.equal(response?.contentType, 'image/png', imageUrl);
    assert.deepEqual(response?.body, Buffer.from('test image'), imageUrl);
  }
});
