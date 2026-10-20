import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { after, test } from 'node:test';
import matter from 'gray-matter';

const runtime = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-flashcards-api-'));
process.env.RBOOK_RUNTIME_DIR = runtime;
fs.mkdirSync(path.join(runtime, 'dist'));
const { setIndexPayload } = await import('../packages/rbook-search/dist/searchIndex.js');
const { createApp } = await import('../packages/rbook-server/dist/app.js');
const app = await createApp({ logger: false });
after(async () => { await app.close(); fs.rmSync(runtime, { recursive: true, force: true }); });

test('flashcard API admits only nonempty intuition and uses stable article identity with optional front metadata', async () => {
  setIndexPayload({
    generatedAt: 'fixture-date',
    pages: [
      { id: 'dijkstra', title: 'Dijkstra: 答案在标题中', url: '/graph/dijkstra/index.html',
        frontMatter: { intuition: ' 非负边权。 ', flashcard: { title: 'Dijkstra', hint: ' 水管 ' }, tags: ['图论', 123] } },
      { id: 'hidden', title: '未列入目录', url: '/hidden.html', visible: false, frontMatter: { intuition: '独立直觉' } },
      { id: 'blank', frontMatter: { intuition: ' \n ' } },
      { id: 'missing', frontMatter: {} },
      { id: 'wrong-type', frontMatter: { intuition: ['不是字符串'] } },
      { id: 'fallback', title: '原文标题', url: '/fallback.html',
        frontMatter: { intuition: '内容', flashcard: { title: false, hint: ['错误类型'] } } }
    ]
  });
  const response = await app.inject('/api/flashcards');
  assert.equal(response.statusCode, 200);
  assert.equal(response.headers['cache-control'], 'no-store');
  assert.deepEqual(response.json(), {
    generatedAt: 'fixture-date', total: 3, items: [
      { id: 'dijkstra', title: 'Dijkstra', hint: '水管', intuition: '非负边权。', tags: ['图论'], url: '/graph/dijkstra/index.html' },
      { id: 'hidden', title: '未列入目录', hint: '', intuition: '独立直觉', tags: [], url: '/hidden.html' },
      { id: 'fallback', title: '原文标题', hint: '', intuition: '内容', tags: [], url: '/fallback.html' }
    ]
  });
});

test('empty indexes yield an empty deck', async () => {
  setIndexPayload({ generatedAt: 'empty', pages: [] });
  const response = await app.inject('/api/flashcards');
  assert.equal(response.statusCode, 200);
  assert.deepEqual(response.json(), { generatedAt: 'empty', total: 0, items: [] });
});

test('the seven seed cards are parsed from real article metadata and their bodies stay outside the API', async () => {
  const files = [
    'graph/dijkstra', 'base/binary_search', 'math/quick_pow', 'sort/quicksort',
    'data_structure/disjoint_set', 'graph/kruskal', 'graph/topsort'
  ];
  const pages = files.map((file) => {
    const { data } = matter(fs.readFileSync(new URL(`../book/pages/${file}/index.md`, import.meta.url), 'utf8'));
    assert.ok(typeof data.intuition === 'string' && data.intuition.trim(), file);
    assert.ok(typeof data.flashcard?.hint === 'string' && data.flashcard.hint.trim(), file);
    return { id: data.id, title: data.title, url: `/${file}/index.html`, frontMatter: data };
  });
  setIndexPayload({ generatedAt: 'seed', pages });
  const { total, items } = (await app.inject('/api/flashcards')).json();
  assert.equal(total, 7);
  assert.equal(new Set(items.map((item) => item.id)).size, 7);
  assert.equal(items[0].title, 'Dijkstra 算法');
  for (let i = 0; i < items.length; i++) {
    assert.equal(items[i].intuition, pages[i].frontMatter.intuition.trim());
    assert.equal(Object.hasOwn(items[i], 'frontMatter'), false);
    assert.equal(Object.hasOwn(items[i], 'markdown'), false);
  }
});
