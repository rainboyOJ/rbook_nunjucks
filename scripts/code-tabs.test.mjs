import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import { load } from 'cheerio';
import { chromium } from 'playwright';
import { compile } from 'sass';
import Markdown from '@rbook/markdown';
import md from '@rbook/markdown/markdown-it';

const root = path.resolve(import.meta.dirname, '..');
const article = new Markdown(path.join(root, 'book/pages/data_structure/BIT/index.md'), {
  baseDir: path.join(root, 'book'),
  codeDir: path.join(root, 'book/code')
}).toJSON().html_content;

const extraGroup = `
:::: code-tabs
::: tab C++

~~~cpp
int first = 1;
~~~
:::
::: tab Python
Second group explanation.

~~~python
second = 2
~~~
:::
::: tab JavaScript
~~~javascript
const third = 3;
~~~
:::
::: tab Haskell
~~~haskell
fourth = 4
~~~
:::
::::
`;

test('BIT includes retain highlighted code and language-specific prose inside panels', () => {
  const $ = load(article);
  const group = $('[data-code-tabs]');
  assert.equal(group.length, 1);
  assert.deepEqual(group.find('.code-tab-list button').map((_, node) => $(node).text()).get(), ['C++', 'Python']);
  const panels = group.children('.code-tab-panel');
  assert.equal(panels.length, 2);
  assert.match(panels.eq(1).text(), /Python 版把同一份接口封装成/);
  assert.match(panels.eq(1).find('pre code').text(), /class Fenwick/);
  assert.ok(panels.eq(0).find('.token').length);
  assert.ok($('pre code').filter((_, node) => $(node).text().includes('int main')).length);
});

test('groups have unique IDs, escaped labels, and stable output across renders', () => {
  const source = extraGroup + extraGroup.replace('tab C++', 'tab <img src=x onerror=alert(1)>');
  const env = {};
  const html = md.render(source, env);
  assert.equal(md.render(source, env), html);
  const $ = load(html);
  const ids = $('[id]').map((_, node) => node.attribs.id).get();
  assert.equal(new Set(ids).size, ids.length);
  assert.equal($('.code-tab-list img').length, 0);
  assert.match($('.code-tab-list').last().text(), /<img src=x onerror=alert\(1\)>/);
  $('.code-tab-list button').each((_, button) => {
    assert.equal($(`[id="${button.attribs['aria-controls']}"]`).length, 1);
  });
  assert.equal($('.code-tab-panel .line-numbers-mode').length, 8);
});

test('nested groups only collect their own tabs; standalone tab content remains readable', () => {
  const $ = load(md.render(`:::::: code-tabs\n::::: tab Outer\n${extraGroup}\n:::::\n::::::\n\n::: tab Standalone\nStill readable.\n:::`));
  const groups = $('[data-code-tabs]');
  assert.equal(groups.length, 2);
  assert.equal(groups.first().children('.code-tab-list').find('button').length, 1);
  assert.equal(groups.last().children('.code-tab-list').find('button').length, 4);
  assert.match($.text(), /Still readable/);
});

test('browser: independent tabs, keyboard, copying, mobile, themes, print and no-JS fallback', async (t) => {
  const browser = await chromium.launch({ headless: true });
  t.after(() => browser.close());
  const page = await browser.newPage({ viewport: { width: 1100, height: 900 } });
  const errors = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const css = compile(path.join(root, 'site/markdown-style/markdown.scss'), {
    loadPaths: [path.join(root, 'packages/rbook-markdown/src/markdown-it/assets')],
    logger: { warn() {}, debug() {} }
  }).css;
  const js = fs.readFileSync(path.join(root, 'site/theme/assets/js/main.js'), 'utf8');
  const baseCss = fs.readFileSync(path.join(root, 'site/theme/assets/css/style.css'), 'utf8');
  // Render as one document so IDs are assigned exactly as on an article page.
  const source = fs.readFileSync(path.join(root, 'book/pages/data_structure/BIT/index.md'), 'utf8');
  const { expandIncludeCode } = await import('@rbook/markdown/include-code');
  const content = md.render(expandIncludeCode(source.slice(source.indexOf('\n---', 3) + 4) + extraGroup, {
    baseDir: path.join(root, 'book'),
    currentFilePath: path.join(root, 'book/pages/data_structure/BIT/index.md')
  }));
  const document = `<html data-darkmode="light"><head><style>${baseCss}\n${css}</style></head>
    <body class="printable-article"><a class="skip-link" href="#main-content">跳到正文</a><main id="main-content"><article class="page reading-page"><div class="container reading-content"><div class="heti markdown-body">${content}</div></div></article></main>
    <script>${js}</script></body></html>`;
  await page.setContent(document);
  const groups = page.locator('[data-code-tabs]');
  const first = groups.nth(0);
  const second = groups.nth(1);
  const panels = first.locator(':scope > .code-tab-panel');
  assert.equal(await first.getByRole('tab', { selected: true }).textContent(), 'C++');
  assert.equal(await panels.nth(1).isVisible(), false);
  await first.getByRole('tab', { name: 'Python', exact: true }).click();
  assert.equal(await panels.nth(0).isVisible(), false);
  assert.equal(await panels.nth(1).isVisible(), true);
  assert.equal(await second.getByRole('tab', { selected: true }).textContent(), 'C++');
  await page.evaluate(() => Object.defineProperty(navigator, 'clipboard', {
    configurable: true, value: { writeText: async (text) => { window.copiedCode = text; } }
  }));
  await panels.nth(1).getByRole('button', { name: '复制代码', exact: true }).click();
  assert.equal(await page.evaluate(() => window.copiedCode), await panels.nth(1).locator('pre code').textContent());
  const pythonTab = first.getByRole('tab', { name: 'Python', exact: true });
  await pythonTab.focus();
  await page.keyboard.press('ArrowRight');
  assert.equal(await first.getByRole('tab', { selected: true }).textContent(), 'C++');
  await page.keyboard.press('ArrowLeft');
  assert.equal(await first.getByRole('tab', { selected: true }).textContent(), 'Python');
  await page.keyboard.press('Home');
  assert.equal(await first.getByRole('tab', { selected: true }).textContent(), 'C++');
  await page.keyboard.press('End');
  assert.equal(await pythonTab.getAttribute('aria-selected'), 'true');
  await page.keyboard.press('Tab');
  assert.equal(await panels.nth(1).evaluate((node) => node === document.activeElement), true);
  await page.setViewportSize({ width: 320, height: 740 });
  const strip = second.locator('.code-tab-list');
  assert.ok(await strip.evaluate((node) => node.scrollWidth > node.clientWidth));
  await second.getByRole('tab', { name: 'Haskell', exact: true }).click();
  assert.equal(await second.getByRole('tabpanel').locator('pre code').textContent(), 'fourth = 4');
  assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= 320));
  const light = await strip.evaluate((node) => getComputedStyle(node).backgroundColor);
  await page.evaluate(() => { document.documentElement.dataset.darkmode = 'dark'; });
  const dark = await strip.evaluate((node) => getComputedStyle(node).backgroundColor);
  assert.notEqual(light, dark);
  await page.evaluate(() => { document.documentElement.dataset.darkmode = 'auto'; });
  await page.emulateMedia({ colorScheme: 'dark' });
  assert.equal(await strip.evaluate((node) => getComputedStyle(node).backgroundColor), dark);
  await page.emulateMedia({ media: 'print' });
  for (const panel of await groups.locator(':scope > .code-tab-panel').all()) {
    assert.equal(await panel.isVisible(), true);
    assert.equal(await panel.locator(':scope > .code-tab-title').isVisible(), true);
    assert.equal(await panel.locator('pre').isVisible(), true);
    assert.equal(await panel.locator('pre').evaluate((node) => getComputedStyle(node).whiteSpace), 'pre-wrap');
  }
  assert.equal(await strip.isVisible(), false);
  assert.equal(await page.locator('.skip-link').isVisible(), false);
  assert.equal(await first.getByText('复制', { exact: true }).first().isVisible(), false);
  await page.emulateMedia({ media: 'screen' });
  assert.equal(await panels.nth(0).isVisible(), false);
  assert.equal(await pythonTab.getAttribute('aria-selected'), 'true');
  const noJs = await browser.newPage({ javaScriptEnabled: false });
  await noJs.setContent(document);
  for (const panel of await noJs.locator('.code-tab-panel').all()) assert.equal(await panel.isVisible(), true);
  assert.equal(await noJs.locator('.code-tab-list').first().isVisible(), false);
  assert.deepEqual(errors, []);
});
