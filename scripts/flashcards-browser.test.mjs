import assert from 'node:assert/strict';
import fs from 'node:fs';
import http from 'node:http';
import { after, before, test } from 'node:test';
import { chromium } from 'playwright';

const html = fs.readFileSync(new URL('../site/widgets/flashcards/index.html', import.meta.url));
const cards = [
  { id: 'a', title: 'Dijkstra 算法', hint: '想想水管。<b>提示是纯文本</b>', intuition: '水第一次到达的时间，就是最短距离。', tags: ['图论'], url: '/graph/dijkstra/index.html' },
  { id: 'b', title: '二分查找', hint: '', intuition: '依据单调性缩小候选区间。', tags: ['基础'], url: '/base/binary_search/index.html' },
  { id: 'c', title: '拓扑排序', hint: '前置任务', intuition: '把没有前置依赖的任务取出来。', tags: ['图论'], url: '/graph/topsort/index.html' }
];
let server, browser, base;
before(async () => {
  server = http.createServer((req, res) => {
    if (req.url === '/api/flashcards') {
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ total: cards.length, items: cards }));
    } else {
      res.setHeader('Content-Type', 'text/html');
      res.end(html);
    }
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  base = `http://127.0.0.1:${server.address().port}`;
  browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH || undefined
  });
});
after(async () => {
  await browser?.close();
  if (server) await new Promise((resolve) => server.close(resolve));
});
async function setup(t, options = {}, payload) {
  const context = await browser.newContext(options);
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', (e) => errors.push(e.message));
  t.after(async () => { await context.close(); assert.deepEqual(errors, []); });
  if (payload) await page.route('**/api/flashcards', (route) => route.fulfill({ json: payload }));
  await page.goto(`${base}/flashcards`);
  return { page, context };
}
async function id(page) { return page.locator('#card').getAttribute('data-id'); }
async function side(page) { return page.locator('#card').getAttribute('data-side'); }
// Navigation that starts on the back face turns the card over first, so settle before asserting.
async function waitId(page, expected) {
  await page.waitForFunction((value) => document.querySelector('#card')?.dataset.id === value, expected);
}

test('front, optional hint, back and source link; buttons and keyboard reset each card to its front', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  assert.equal(await id(page), 'a');
  assert.equal(await page.locator('#back').isVisible(), false);
  assert.equal(await page.locator('#article-link').isVisible(), false);
  await page.locator('#hint-toggle').click();
  assert.equal(await side(page), 'front');
  assert.equal(await page.locator('#hint').textContent(), cards[0].hint);
  assert.equal(await page.locator('#hint b').count(), 0);
  await page.locator('#front-title').click();
  assert.equal(await side(page), 'back');
  assert.equal(await page.locator('#intuition').textContent(), cards[0].intuition);
  assert.equal(await page.locator('#hint').isVisible(), false);
  assert.equal(await page.locator('#article-link').getAttribute('target'), '_blank');
  const popupPromise = page.waitForEvent('popup');
  await page.locator('#article-link').click();
  const popup = await popupPromise;
  await popup.waitForLoadState();
  assert.equal(popup.url(), `${base}${cards[0].url}`);
  assert.equal(await side(page), 'back', 'opening the article never flips');
  await popup.close();
  await page.locator('#next').click();
  await waitId(page, 'b');
  assert.equal(await side(page), 'front');
  assert.equal(await page.locator('#back').isVisible(), false, 'only the settled face stays rendered');
  assert.equal(await page.locator('#front').isVisible(), true);
  assert.equal(await page.locator('#hint-toggle').isVisible(), false);
  // Direction shortcuts also work after a navigation button has focus.
  await page.keyboard.press('ArrowRight');
  await waitId(page, 'c');
  await page.locator('#card').focus();
  await page.keyboard.press('Space');
  assert.equal(await side(page), 'back');
  await page.keyboard.press('Enter');
  assert.equal(await side(page), 'front');
  await page.keyboard.press('ArrowLeft');
  await waitId(page, 'b');
  await page.locator('#flip').focus();
  await page.keyboard.press('Space');
  assert.equal(await side(page), 'back', 'native button space flips exactly once');
});

test('random covers each round without repeats, includes manual visits, and refresh discards all state', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  await page.locator('#next').click(); // a and b have been seen
  await page.locator('#random').click();
  assert.equal(await id(page), 'c', 'manual visits are excluded from random candidates');
  assert.match(await page.locator('#round').textContent(), /第 1 轮 · 已看 3 \/ 3/);
  let previous = 'c';
  for (let round = 2; round <= 5; round++) {
    const seen = new Set();
    for (let i = 0; i < cards.length; i++) {
      await page.locator('#random').click();
      const current = await id(page);
      assert.notEqual(current, previous);
      assert.equal(seen.has(current), false);
      seen.add(current);
      previous = current;
      assert.equal(await side(page), 'front');
    }
    assert.match(await page.locator('#round').textContent(), new RegExp(`第 ${round} 轮 · 已看 3 / 3`));
  }
  await page.locator('#flip').click();
  assert.deepEqual(await page.evaluate(() => [localStorage.length, sessionStorage.length]), [0, 0]);
  await page.reload();
  await page.locator('#card').waitFor();
  assert.equal(await id(page), 'a');
  assert.equal(await side(page), 'front');
  assert.match(await page.locator('#round').textContent(), /第 1 轮 · 已看 1 \/ 3/);
});

test('tag changes reset a round and constrain all navigation; single-card decks remain usable', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  await page.locator('#tag').selectOption('图论');
  assert.equal(await page.locator('#total').textContent(), '2');
  await page.locator('#previous').click();
  assert.equal(await id(page), 'c');
  await page.locator('#next').click();
  assert.equal(await id(page), 'a');
  await page.locator('#random').click();
  assert.equal(await id(page), 'c');
  await page.locator('#tag').selectOption('基础');
  assert.equal(await id(page), 'b');
  assert.match(await page.locator('#round').textContent(), /第 1 轮 · 已看 1 \/ 1/);
  for (const action of ['previous', 'next', 'random']) assert.equal(await page.locator(`#${action}`).isDisabled(), true);
  await page.locator('#flip').click();
  assert.equal(await side(page), 'back');
  await page.locator('#tag').selectOption('');
  assert.equal(await side(page), 'front');
  assert.equal(await page.locator('#total').textContent(), '3');
  assert.match(await page.locator('#round').textContent(), /第 1 轮 · 已看 1 \/ 3/);
});

test('desktop drags switch cards, vertical drags do not, and selected answer text does not flip', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  const box = await page.locator('#card').boundingBox();
  async function drag(dx, dy, steps = 12) {
    await page.mouse.move(box.x + box.width * .7, box.y + 15);
    await page.mouse.down();
    await page.mouse.move(box.x + box.width * .7 + dx, box.y + 15 + dy, { steps });
    await page.mouse.up();
  }
  // Under the distance threshold and slow enough not to count as a flick: the card springs back.
  await page.mouse.move(box.x + box.width * .7, box.y + 15);
  await page.mouse.down();
  for (let step = 1; step <= 12; step++) {
    await page.mouse.move(box.x + box.width * .7 - step * 8, box.y + 15);
    await page.waitForTimeout(16);
  }
  assert.match(await page.locator('#slide').evaluate((el) => el.style.transform), /translateX\(-9[0-6]px\)/, 'the card follows the drag');
  await page.mouse.up();
  await page.waitForTimeout(240);
  assert.equal(await id(page), 'a');
  assert.equal(await page.locator('#slide').evaluate((el) => el.style.transform), '');
  assert.equal(await page.locator('#slide').evaluate((el) => el.style.opacity), '');
  await drag(-box.width * .4, 0);
  assert.equal(await id(page), 'b');
  assert.equal(await side(page), 'front', 'the click following a swipe must not flip the new card');
  await drag(box.width * .4, 0);
  assert.equal(await id(page), 'a');
  await drag(0, 120);
  assert.equal(await id(page), 'a');
  assert.equal(await side(page), 'front');
  await page.locator('#flip').click();
  await page.locator('#intuition').evaluate((node) => {
    const selection = window.getSelection();
    const range = document.createRange();
    range.selectNodeContents(node);
    selection.removeAllRanges();
    selection.addRange(range);
  });
  await page.locator('#card').dispatchEvent('click');
  assert.equal(await side(page), 'back');
});

test('mobile touch swipes, long cards, reduced motion and literal text have no overflow or hidden answer leaks', async (t) => {
  const long = { ...cards[0], title: '算法名称'.repeat(10), intuition: '<img src=x onerror="alert(1)">' + '较长的核心直觉解释。'.repeat(60) };
  const { page } = await setup(t, { viewport: { width: 320, height: 740 }, isMobile: true, hasTouch: true, reducedMotion: 'reduce' },
    { items: [long, cards[1]], total: 2 });
  await page.locator('#card').waitFor();
  // Chromium's real touch input exercises touch-action and compatibility click suppression.
  const client = await page.context().newCDPSession(page);
  await page.locator('#card').scrollIntoViewIfNeeded();
  const box = await page.locator('#card').boundingBox();
  const x = box.x + box.width - 30, y = box.y + 80;
  await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x, y }] });
  await client.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x: x - 130, y }] });
  await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
  await page.waitForFunction(() => document.querySelector('#card').dataset.id === 'b');
  assert.equal(await id(page), 'b');
  assert.equal(await side(page), 'front');
  await page.locator('#front-title').tap();
  assert.equal(await side(page), 'back', 'a separate tap reveals the new card');
  await page.locator('#previous').tap();
  await page.waitForFunction(() => document.querySelector('#card').dataset.id === 'a');
  await page.locator('#flip').tap();
  assert.equal(await page.locator('#intuition img').count(), 0);
  assert.equal(await page.locator('#intuition').textContent(), long.intuition);
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
  assert.equal(await page.locator('#card').evaluate((el) => getComputedStyle(el).animationName), 'none');
  await page.locator('#back').scrollIntoViewIfNeeded();
  const back = await page.locator('#back').boundingBox();
  const scrollX = back.x + back.width / 2, scrollY = back.y + back.height - 35;
  await client.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: scrollX, y: scrollY }] });
  await client.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x: scrollX, y: scrollY - 120 }] });
  await client.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] });
  await page.waitForFunction(() => document.querySelector('#back').scrollTop > 0);
  assert.equal(await side(page), 'back', 'vertical reading scroll never flips or changes the card');
  assert.equal(await id(page), 'a');
  await page.locator('#article-link').scrollIntoViewIfNeeded();
  assert.equal(await page.locator('#article-link').isVisible(), true);
});

test('empty and malformed cards are excluded, while unsafe source URLs are not clickable', async (t) => {
  const { page } = await setup(t, {}, { items: [null, { ...cards[0], intuition: ' ' }, { ...cards[1], intuition: ['wrong'] }] });
  await page.locator('#notice-text').getByText('还没有可用的算法卡片', { exact: false }).waitFor();
  assert.equal(await page.locator('#card').isVisible(), false);
  assert.equal(await page.locator('#flip').isDisabled(), true);
  await page.route('**/api/flashcards', (route) => route.fulfill({ json: { items: [{ ...cards[0], url: 'javascript:alert(1)' }] } }));
  await page.reload();
  await page.locator('#card').waitFor();
  await page.locator('#flip').click();
  assert.equal(await page.locator('#article-link').isVisible(), false);
  assert.equal(await page.locator('#article-link').getAttribute('href'), null);
});

test('failed API requests show retry and can recover without reloading the page', async (t) => {
  const { page } = await setup(t, {}, { items: [] });
  await page.route('**/api/flashcards', (route) => route.fulfill({ status: 503, body: 'Unavailable' }));
  await page.reload();
  await page.locator('#retry').waitFor();
  assert.match(await page.locator('#notice-text').textContent(), /加载失败/);
  await page.unroute('**/api/flashcards');
  await page.locator('#retry').click();
  await page.locator('#card').waitFor();
  assert.equal(await id(page), 'a');
  assert.equal(await page.locator('#retry').isVisible(), false);
});

test('the card turns over in 3D and only the settled face stays rendered', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  const stage = page.locator('#stage');
  assert.equal(await stage.evaluate((el) => getComputedStyle(el).transitionProperty), 'transform');
  assert.equal(await stage.evaluate((el) => getComputedStyle(el).transitionDuration), '0.42s');
  assert.equal(await stage.evaluate((el) => getComputedStyle(el).transformStyle), 'preserve-3d');
  await page.locator('#flip').click();
  assert.equal(await side(page), 'back');
  // Both faces are on stage while turning, so the answer is not popped in by a display switch.
  assert.equal(await page.locator('#back-face').isVisible(), true);
  assert.equal(await page.locator('#front-face').isVisible(), true);
  await page.waitForFunction(() => document.querySelector('#front-face').hidden === true);
  assert.match(await stage.evaluate((el) => getComputedStyle(el).transform), /^matrix/, 'the stage rests at half a turn');
  assert.equal(await page.locator('#front-face').evaluate((el) => el.inert), true);
  assert.equal(await page.locator('#back-face').evaluate((el) => el.inert), false);
  assert.equal(await page.locator('#front-face').getAttribute('aria-hidden'), 'true');
  assert.equal(await page.locator('#back-face').getAttribute('aria-hidden'), 'false');
  await page.locator('#flip').click();
  await page.waitForFunction(() => document.querySelector('#back-face').hidden === true);
  assert.equal(await page.locator('#front-face').evaluate((el) => el.inert), false);
});

test('each navigation pushes a cloned card out and removes it afterwards', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  await page.locator('#next').click();
  assert.equal(await page.evaluate(() => document.querySelectorAll('.ghost').length), 1);
  assert.equal(await page.locator('#card').count(), 1, 'the clone drops ids so #card stays unique');
  assert.deepEqual(
    await page.evaluate(() => document.querySelector('.ghost').getAnimations().map((animation) => animation.effect.getKeyframes().at(-1).transform)),
    ['translateX(-100%) rotate(-5deg)']
  );
  assert.equal(await page.locator('#slide').evaluate((el) => el.getAnimations().length), 1, 'the next card slides in');
  await waitId(page, 'b');
  await page.waitForFunction(() => document.querySelectorAll('.ghost').length === 0);
  await page.locator('#previous').click();
  assert.deepEqual(
    await page.evaluate(() => document.querySelector('.ghost').getAnimations().map((animation) => animation.effect.getKeyframes().at(-1).transform)),
    ['translateX(100%) rotate(5deg)']
  );
  await waitId(page, 'a');
});

test('a quick second click interrupts the same motion instead of queueing it', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  await page.locator('#next').click();
  await page.locator('#next').click();
  await waitId(page, 'c');
  assert.equal(await page.evaluate(() => document.querySelectorAll('.ghost').length <= 1), true, 'only the latest clone survives');
  await page.waitForFunction(() => document.querySelectorAll('.ghost').length === 0);
});

test('dragging a revealed answer turns it back over before the swipe commits', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  const box = await page.locator('#card').boundingBox();
  await page.locator('#flip').click();
  assert.equal(await side(page), 'back');
  await page.mouse.move(box.x + box.width * .7, box.y + 15);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width * .7 - box.width * .5, box.y + 15, { steps: 12 });
  assert.equal(await side(page), 'front', 'the answer leaves face up');
  await page.mouse.up();
  await waitId(page, 'b');
});

test('the deck edge decoration follows the size of the current range', async (t) => {
  const { page } = await setup(t);
  await page.locator('#card').waitFor();
  assert.equal(await page.locator('.deck-layer').count(), 2);
  assert.equal(await page.locator('#deck').getAttribute('data-stack'), 'on');
  assert.equal(await page.locator('.deck-layer').first().evaluate((el) => getComputedStyle(el).pointerEvents), 'none');
  await page.locator('#tag').selectOption('基础');
  assert.equal(await page.locator('#deck').getAttribute('data-stack'), 'off');
  assert.equal(await page.locator('.deck-layer').first().isVisible(), false);
});

test('reduced motion swaps faces and cards instantly, without any animation work', async (t) => {
  const { page } = await setup(t, { reducedMotion: 'reduce' });
  await page.locator('#card').waitFor();
  await page.locator('#flip').click();
  assert.equal(await side(page), 'back');
  assert.equal(await page.locator('#back-face').isVisible(), true);
  assert.equal(await page.locator('#front-face').isVisible(), false, 'nothing to wait for');
  assert.equal(await page.evaluate(() => document.getAnimations().length), 0);
  await page.locator('#next').click();
  assert.equal(await id(page), 'b', 'navigation lands in the same click');
  assert.equal(await page.evaluate(() => document.querySelectorAll('.ghost').length), 0);
  assert.equal(await page.evaluate(() => document.getAnimations().length), 0);
});
