import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const navigationPath = path.join(root, 'site/theme/partials/app_nav_bar.pug');

test('homepage navigation exposes the reader widgets in the shared order', () => {
  const source = fs.readFileSync(navigationPath, 'utf8');
  const expected = [
    ['/', '首页'],
    ['/explore', '文章探索'],
    ['/code_template', '代码模板'],
    ['/tags', '标签'],
    ['/relations', '关系'],
    ['/practice', '学习队列']
  ];

  let previousPosition = -1;
  for (const [href, label] of expected) {
    const linkPosition = source.indexOf(`a(href="${href}")`);
    assert.ok(linkPosition > previousPosition, `${href} should follow the previous navigation item`);
    const labelPosition = source.indexOf(`| ${label}`, linkPosition);
    assert.ok(labelPosition > linkPosition, `${href} should be labeled ${label}`);
    previousPosition = labelPosition;
  }

  assert.equal(source.includes('href="/problem_sets/index.html"'), false);
});
