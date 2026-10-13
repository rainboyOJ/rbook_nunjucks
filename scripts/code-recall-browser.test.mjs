import assert from 'node:assert/strict';
import fs from 'node:fs';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import { after, before, test } from 'node:test';
import { chromium } from 'playwright';

const html = fs.readFileSync(new URL('../site/widgets/code_template_filter/index.html', import.meta.url));
const key = 'rbook.code-recall.v1';
const templates = [
  { id: 'sample-cpp', description: '并查集练习', language: 'cpp', recall_prompt: '实现初始化、查找与合并。\n编号从 1 开始。<b>这是纯文本</b>', content: 'int find(int x) { return p[x] == x ? x : p[x] = find(p[x]); }' },
  { id: 'sample-py', description: 'Python 模板', language: 'py', content: 'print("reference")' },
  { id: 'sample-extra', description: '第三张卡', language: 'cpp', content: 'int n;' }
].map((item) => ({ tags: [], articles: [], path: `${item.id}.${item.language}`, ...item }));
let browser;
let server;
let base;

before(async () => {
  server = http.createServer((req, res) => {
    const url = new URL(req.url, 'http://localhost');
    if (url.pathname === '/api/codes') {
      res.setHeader('Content-Type', 'application/json');
      const id = url.searchParams.get('id');
      res.end(JSON.stringify(id ? templates.find((item) => item.id === id)
        : { total: templates.length, items: templates.map(({ content, ...item }) => item) }));
    } else {
      res.setHeader('Content-Type', 'text/html');
      res.end(html);
    }
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  base = `http://127.0.0.1:${server.address().port}`;
  browser = await chromium.launch({ headless: true });
});
after(async () => {
  await browser?.close();
  if (server) await new Promise((resolve) => server.close(resolve));
});

async function setup(t, viewport = { width: 1280, height: 1000 }) {
  const context = await browser.newContext({ viewport, timezoneId: 'Asia/Shanghai' });
  await context.route('https://cdnjs.cloudflare.com/**', (route) => route.abort());
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  t.after(async () => { await context.close(); assert.deepEqual(errors, []); });
  await page.goto(base);
  await page.locator('[data-action="deck"]').first().waitFor();
  return { page, context };
}

async function add(page, id = 'sample-cpp') {
  await page.locator('#library-tab').click();
  await page.locator(`[data-action="deck"][data-id="${id}"]`).click();
}
async function draw(page) {
  await page.locator('#study-tab').click();
  await page.locator('#draw-card').click();
  await page.locator('#recall-card').waitFor();
}
async function reveal(page) {
  await page.locator('#reveal-answer').click();
  await page.locator('#recall-ratings').waitFor();
}
async function data(page) {
  return page.evaluate((storageKey) => JSON.parse(localStorage.getItem(storageKey)), key);
}

test('add, draw, persist draft across reload, compare, rate, and restore exported backup', async (t) => {
  const { page } = await setup(t);
  await add(page);
  await draw(page);
  assert.equal(await page.locator('#recall-editor').inputValue(), '');
  assert.equal(await page.locator('#reference-column').isVisible(), false);
  assert.equal(await page.locator('#recall-ratings').isVisible(), false);
  assert.equal(await page.locator('#recall-prompt').textContent(), templates[0].recall_prompt);
  assert.equal(await page.locator('#recall-prompt b').count(), 0, 'prompt is text, never HTML');
  await page.locator('#recall-editor').fill('int find(int x) { return x; }');
  await page.reload();
  await page.locator('#recall-card').waitFor();
  assert.equal(await page.locator('#recall-editor').inputValue(), 'int find(int x) { return x; }');
  await reveal(page);
  assert.equal(await page.locator('#reference-code').textContent(), templates[0].content);
  assert.equal(await page.locator('#recall-editor').getAttribute('readonly'), '');
  const positions = await page.locator('.recall-column').evaluateAll((nodes) => nodes.map((n) => ({ x: n.getBoundingClientRect().x, y: n.getBoundingClientRect().y })));
  assert.equal(positions[0].y, positions[1].y);
  assert.ok(positions[1].x > positions[0].x);
  await page.locator('[data-rating="good"]').click();
  const saved = await data(page);
  assert.equal(saved.cards[0].stage, 1);
  assert.equal(saved.active, null);
  assert.equal(saved.daily.count, 1);
  assert.equal(await page.locator('#draw-card').isDisabled(), true);

  await page.locator('.study-settings summary').click();
  const downloadPromise = page.waitForEvent('download');
  await page.locator('#export-study').click();
  const download = await downloadPromise;
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-recall-backup-'));
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));
  const backup = path.join(dir, download.suggestedFilename());
  await download.saveAs(backup);
  assert.deepEqual(JSON.parse(fs.readFileSync(backup, 'utf8')), saved);

  await add(page, 'sample-py');
  await page.locator('#study-tab').click();
  page.once('dialog', (dialog) => dialog.accept());
  await page.locator('#import-file').setInputFiles(backup);
  await page.waitForFunction((storageKey) => JSON.parse(localStorage.getItem(storageKey)).cards.length === 1, key);
  assert.deepEqual(await data(page), saved);
  const screenshot = process.env.RBOOK_RECALL_SCREENSHOT;
  if (screenshot) await page.screenshot({ path: screenshot, fullPage: true });
});

test('failure waits, survives reload, and retries from blank without using another new slot', async (t) => {
  const { page } = await setup(t);
  await page.clock.install({ time: new Date('2026-10-13T10:00:00+08:00') });
  await add(page);
  await draw(page);
  await page.locator('#recall-editor').fill('unfinished');
  await reveal(page);
  await page.locator('[data-rating="again"]').click();
  assert.equal(await page.locator('#draw-card').isDisabled(), true);
  await page.reload();
  await page.locator('#study-tab').click();
  assert.equal(await page.locator('#draw-card').isDisabled(), true);
  await page.clock.fastForward(61_000);
  await page.locator('#draw-card').click();
  assert.equal(await page.locator('#recall-editor').inputValue(), '');
  assert.equal(await page.locator('#reference-column').isVisible(), false);
  assert.equal((await data(page)).daily.count, 1);
  await reveal(page);
  await page.locator('[data-rating="hard"]').click();
  assert.equal((await data(page)).cards[0].stage, 0);
  assert.equal((await data(page)).cards[0].due, '2026-10-14');
});

test('mobile name-only cards stack answers, honor zero quota, and keep invalid imports from altering progress', async (t) => {
  const { page } = await setup(t, { width: 375, height: 850 });
  await add(page, 'sample-py');
  await page.locator('#study-tab').click();
  await page.locator('.study-settings summary').click();
  await page.locator('#new-limit').fill('0');
  await page.locator('#new-limit').press('Tab');
  assert.equal(await page.locator('#draw-card').isDisabled(), true);
  await page.locator('#new-limit').fill('1');
  await page.locator('#new-limit').press('Tab');
  await page.locator('#draw-card').click();
  assert.equal(await page.locator('#recall-prompt').isVisible(), false);
  await page.locator('#recall-editor').fill('print("mine")');
  await reveal(page);
  const columns = await page.locator('.recall-column').evaluateAll((nodes) => nodes.map((n) => ({ x: n.getBoundingClientRect().x, y: n.getBoundingClientRect().y })));
  assert.equal(columns[0].x, columns[1].x);
  assert.ok(columns[1].y > columns[0].y);
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
  const saved = await data(page);
  await page.locator('#import-file').setInputFiles({ name: 'invalid.json', mimeType: 'application/json', buffer: Buffer.from('{"version":99}') });
  await page.locator('#study-error').waitFor();
  assert.deepEqual(await data(page), saved);
  const screenshot = process.env.RBOOK_RECALL_MOBILE_SCREENSHOT;
  if (screenshot) await page.screenshot({ path: screenshot, fullPage: true });
});

test('reference network failure keeps the draft and permits retry; storage failure never advances a rating', async (t) => {
  const { page } = await setup(t);
  await add(page);
  await draw(page);
  await page.locator('#recall-editor').fill('my work');
  await page.route('**/api/codes?**', (route) => route.fulfill({ status: 503, json: { message: 'test unavailable' } }));
  await page.locator('#reveal-answer').click();
  await page.locator('#study-error').waitFor();
  assert.equal(await page.locator('#recall-editor').inputValue(), 'my work');
  assert.equal((await data(page)).active.revealed, false);
  await page.unroute('**/api/codes?**');
  await reveal(page);
  const saved = await data(page);
  await page.evaluate(() => { Storage.prototype.setItem = () => { throw new DOMException('Quota exceeded', 'QuotaExceededError'); }; });
  await page.locator('[data-rating="good"]').click();
  assert.deepEqual(await data(page), saved);
  assert.equal(await page.locator('#recall-ratings').isVisible(), true);
  assert.match(await page.locator('#study-error').textContent(), /保存未完成/);
});

test('corrupt saved records stay intact and other tabs receive deck updates', async (t) => {
  const { page, context } = await setup(t);
  await page.evaluate((storageKey) => localStorage.setItem(storageKey, 'broken-json'), key);
  await page.reload();
  await page.locator('#study-tab').click();
  await page.locator('#study-error').waitFor();
  await page.locator('#library-tab').click();
  await page.locator('[data-action="deck"]').first().click();
  assert.equal(await page.evaluate((storageKey) => localStorage.getItem(storageKey), key), 'broken-json');
  await page.evaluate((storageKey) => localStorage.removeItem(storageKey), key);
  await page.reload();
  const second = await context.newPage();
  await second.goto(base);
  await second.locator('[data-action="deck"]').first().waitFor();
  await add(page);
  await second.waitForFunction(() => document.querySelector('#study-tab').textContent.includes('1'));
  await add(second, 'sample-py');
  await page.waitForFunction(() => document.querySelector('#study-tab').textContent.includes('2'));
  assert.equal((await data(page)).cards.length, 2);
});
