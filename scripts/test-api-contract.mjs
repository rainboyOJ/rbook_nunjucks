import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const runtimeDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-api-contract-'));
const staticDir = path.join(runtimeDir, 'dist');

// Path modules read this value during import, so isolate the runtime before loading packages.
process.env.RBOOK_RUNTIME_DIR = runtimeDir;
fs.mkdirSync(staticDir, { recursive: true });

function parseJson(response) {
  assert.match(
    response.headers['content-type'] || '',
    /^application\/json/,
    `expected JSON response, got ${response.headers['content-type']}`
  );
  return JSON.parse(response.body);
}

function assertApiResponse(response, statusCode) {
  assert.equal(response.statusCode, statusCode);
  assert.equal(response.headers['cache-control'], 'no-store');
}

function assertRelativeUrl(url, label) {
  assert.equal(typeof url, 'string', `${label} should be a string`);
  assert.equal(url.startsWith('/'), true, `${label} should be root-relative`);
  assert.equal(/^https?:\/\//.test(url), false, `${label} should not be absolute`);
}

function assertPayloadUrls(value, label = '$') {
  if (!value || typeof value !== 'object') return;
  if (Array.isArray(value)) {
    value.forEach((item, index) => assertPayloadUrls(item, `${label}[${index}]`));
    return;
  }

  for (const [key, child] of Object.entries(value)) {
    if (key === 'url' && typeof child === 'string') {
      assertRelativeUrl(child, `${label}.${key}`);
    }
    assertPayloadUrls(child, `${label}.${key}`);
  }
}

function assertNoLocalLeak(value, label) {
  const serialized = JSON.stringify(value);
  assert.equal(serialized.includes('127.0.0.1'), false, `${label} leaks a loopback URL`);
  assert.equal(serialized.includes(rootDir), false, `${label} leaks the repository path`);
}

function apiUrl(pathname, query = {}) {
  const params = new URLSearchParams(query);
  const suffix = params.toString();
  return suffix ? `${pathname}?${suffix}` : pathname;
}

async function main() {
  const [{ buildSearchIndex }, { getIndexPayload }, { createApp }] = await Promise.all([
    import('../packages/rbook-search/dist/buildIndex.js'),
    import('../packages/rbook-search/dist/searchIndex.js'),
    import('../packages/rbook-server/dist/app.js')
  ]);

  const builtIndex = buildSearchIndex();
  assert.equal(builtIndex.stats.errors, 0, 'isolated search index should build cleanly');
  assert.equal(builtIndex.version, 3);
  assert.equal(Object.hasOwn(builtIndex.stats, 'chunks'), false);
  assert.equal(Object.hasOwn(builtIndex, 'chunks'), false);
  assert.equal(Object.hasOwn(builtIndex, 'fuseIndex'), false);

  const indexPath = path.join(runtimeDir, '.search/index.json');
  fs.writeFileSync(indexPath, JSON.stringify({
    ...builtIndex,
    version: 2,
    generatedAt: 'stale-index',
    stats: { ...builtIndex.stats, chunks: 1 },
    chunks: [{ id: 'stale-chunk' }],
    fuseIndex: {}
  }));

  const app = await createApp({ logger: false, staticDir });
  const currentIndex = getIndexPayload();
  assert.equal(currentIndex.version, 3, 'version 2 index should be rebuilt');
  assert.equal(Object.hasOwn(currentIndex.stats, 'chunks'), false);
  assert.equal(Object.hasOwn(currentIndex, 'chunks'), false);
  assert.equal(Object.hasOwn(currentIndex, 'fuseIndex'), false);

  try {
    const docsSource = fs.readFileSync(path.join(rootDir, 'docs/api-usage.md'), 'utf8');

    const docsHtmlResponse = await app.inject({
      method: 'GET',
      url: '/api',
      headers: {
        host: 'docs.example.test',
        'x-forwarded-proto': 'https'
      }
    });
    assertApiResponse(docsHtmlResponse, 200);
    assert.match(docsHtmlResponse.headers['content-type'] || '', /^text\/html/);
    assert.ok(docsHtmlResponse.body.includes('<h1>Rbook HTTP API 使用指南</h1>'));
    assert.ok(docsHtmlResponse.body.includes('https://docs.example.test/api/health'));
    assert.equal(docsHtmlResponse.body.includes('$BASE_URL'), false);

    const docsMarkdownResponse = await app.inject('/api/help?format=md');
    assertApiResponse(docsMarkdownResponse, 200);
    assert.match(docsMarkdownResponse.headers['content-type'] || '', /^text\/markdown/);
    assert.equal(docsMarkdownResponse.body, docsSource);
    assert.ok(docsMarkdownResponse.body.includes('$BASE_URL'));

    const helpHtmlResponse = await app.inject({
      method: 'GET',
      url: '/api/help',
      headers: { host: 'help.example.test' }
    });
    assertApiResponse(helpHtmlResponse, 200);
    assert.match(helpHtmlResponse.headers['content-type'] || '', /^text\/html/);
    assert.ok(helpHtmlResponse.body.includes('<h1>Rbook HTTP API 使用指南</h1>'));
    assert.equal(helpHtmlResponse.body.includes('$BASE_URL'), false);

    const healthResponse = await app.inject('/api/health');
    assertApiResponse(healthResponse, 200);
    const health = parseJson(healthResponse);
    assert.equal(health.ok, true);
    assert.equal(typeof health.generatedAt, 'string');
    assert.deepEqual(health.stats, currentIndex.stats);

    const siteResponse = await app.inject('/api/site');
    assertApiResponse(siteResponse, 200);
    const site = parseJson(siteResponse);
    assert.equal(site.site.title, '我的算法书');
    assert.deepEqual(site.stats, currentIndex.stats);
    assert.equal(site.generatedAt, currentIndex.generatedAt);
    assertNoLocalLeak(site, 'site response');

    const catalogResponse = await app.inject('/api/catalog?compact=true');
    assertApiResponse(catalogResponse, 200);
    const catalog = parseJson(catalogResponse);
    assert.ok(catalog.total > 0, 'catalog should contain visible pages');
    assert.equal(catalog.items.length, catalog.total);
    assert.equal(catalog.items.every((item) => Object.hasOwn(item, 'headings') === false), true);
    const dsuCatalogPage = catalog.items.find((item) => item.id === 'dsu-on-tree');
    assert.ok(dsuCatalogPage, 'catalog should include dsu-on-tree');
    assert.equal(dsuCatalogPage.description, '');
    assertRelativeUrl(dsuCatalogPage.url, 'catalog page URL');
    assertPayloadUrls(catalog);
    assertNoLocalLeak(catalog, 'catalog response');

    const pageResponse = await app.inject('/api/pages?id=dsu-on-tree');
    assertApiResponse(pageResponse, 200);
    const page = parseJson(pageResponse);
    assert.equal(page.id, 'dsu-on-tree');
    assert.equal(page.path, 'algorithm/dsu_on_tree/index.md');
    assertRelativeUrl(page.url, 'page URL');
    assert.equal(typeof page.markdown, 'string');
    assert.ok(page.markdown.length > 0);
    assert.match(page.markdown, /^---\nid: "dsu-on-tree"/);
    assert.equal(page.markdown.includes('@include-code('), false);
    assert.match(page.markdown, /```cpp\n#include <bits\/stdc\+\+\.h>/);
    assert.equal(page.description, '');
    assert.equal(page.frontMatter.id, 'dsu-on-tree');
    assert.deepEqual(page.frontMatter.code_template, ['dsu-on-tree-color-count']);
    assert.deepEqual(Object.keys(page).sort(), [
      'categories',
      'description',
      'frontMatter',
      'headings',
      'id',
      'markdown',
      'navTrail',
      'path',
      'tags',
      'title',
      'url'
    ].sort());
    assertPayloadUrls(page);
    assertNoLocalLeak(page, 'page response');

    const describedPageResponse = await app.inject('/api/pages?id=jump-lca');
    assertApiResponse(describedPageResponse, 200);
    const describedPage = parseJson(describedPageResponse);
    assert.equal(
      describedPage.description,
      '通过倍增预处理祖先表，在 O(log n) 时间内查询两个节点的最近公共祖先。'
    );
    assert.match(describedPage.markdown, /^---\nid: "jump-lca"/);
    assert.equal(describedPage.markdown.includes('@include-code('), false);
    assert.match(describedPage.markdown, /```cpp\n#include <bits\/stdc\+\+\.h>/);

    const codeIdPageResponse = await app.inject('/api/pages?id=ek');
    assertApiResponse(codeIdPageResponse, 200);
    const codeIdPage = parseJson(codeIdPageResponse);
    assert.equal(codeIdPage.markdown.includes('@include-code(maxflow-ek, cpp)'), false);
    assert.match(codeIdPage.markdown, /```cpp\n\/\*\*\n \* Author by Rainboy blog:/);

    const taggedPagesResponse = await app.inject(apiUrl('/api/pages', {
      tag: '树上算法',
      limit: '1',
      offset: '0'
    }));
    assertApiResponse(taggedPagesResponse, 200);
    const taggedPages = parseJson(taggedPagesResponse);
    assert.ok(taggedPages.total >= 1);
    assert.equal(taggedPages.items.length, 1);
    assert.equal(taggedPages.items[0].tags.includes('树上算法'), true);

    const dsuListResponse = await app.inject(apiUrl('/api/pages', {
      tag: '树上算法',
      limit: '50',
      offset: '0'
    }));
    assertApiResponse(dsuListResponse, 200);
    const dsuList = parseJson(dsuListResponse);
    assert.equal(dsuList.items.find((item) => item.id === 'dsu-on-tree')?.description, '');

    const pagedPagesResponse = await app.inject('/api/pages?limit=2&offset=1');
    assertApiResponse(pagedPagesResponse, 200);
    const pagedPages = parseJson(pagedPagesResponse);
    assert.equal(pagedPages.total, currentIndex.pages.length);
    assert.deepEqual(
      pagedPages.items.map((item) => item.id),
      currentIndex.pages.slice(1, 3).map((item) => item.id)
    );

    const codeResponse = await app.inject(
      '/api/codes?id=dsu-on-tree-color-count&includeContent=true'
    );
    assertApiResponse(codeResponse, 200);
    const code = parseJson(codeResponse);
    assert.equal(code.id, 'dsu-on-tree-color-count');
    assert.equal(code.path, 'tree/dsu_on_tree_color_count.cpp');
    assertRelativeUrl(code.url, 'code URL');
    assert.equal(typeof code.content, 'string');
    assert.ok(code.content.length > 100);
    assert.ok(code.articles.some((article) => article.id === 'dsu-on-tree'));
    assertPayloadUrls(code);
    assertNoLocalLeak(code, 'code response');

    const taggedCodesResponse = await app.inject(apiUrl('/api/codes', {
      tag: '树形数据结构',
      limit: '1',
      offset: '0'
    }));
    assertApiResponse(taggedCodesResponse, 200);
    const taggedCodes = parseJson(taggedCodesResponse);
    assert.ok(taggedCodes.total >= 1);
    assert.equal(taggedCodes.items.length, 1);
    assert.equal(taggedCodes.items[0].tags.includes('树形数据结构'), true);

    const pagedCodesResponse = await app.inject('/api/codes?limit=2&offset=1');
    assertApiResponse(pagedCodesResponse, 200);
    const pagedCodes = parseJson(pagedCodesResponse);
    assert.equal(pagedCodes.total, currentIndex.codes.length);
    assert.deepEqual(
      pagedCodes.items.map((item) => item.id),
      currentIndex.codes.slice(1, 3).map((item) => item.id)
    );

    const tagsResponse = await app.inject('/api/tags');
    assertApiResponse(tagsResponse, 200);
    const tags = parseJson(tagsResponse);
    assert.ok(tags.articleTags.some((item) => item.tag === '树上算法' && item.count > 0));
    assert.ok(tags.codeTags.some((item) => item.tag === '树形数据结构' && item.count > 0));
    assertNoLocalLeak(tags, 'tags response');

    const missingPageResponse = await app.inject('/api/pages?id=missing-page');
    assertApiResponse(missingPageResponse, 404);
    assert.equal(parseJson(missingPageResponse).error, 'PAGE_NOT_FOUND');

    const missingCodeResponse = await app.inject('/api/codes?id=missing-code');
    assertApiResponse(missingCodeResponse, 404);
    assert.equal(parseJson(missingCodeResponse).error, 'CODE_NOT_FOUND');

    const unknownResponse = await app.inject('/api/not-a-route');
    assertApiResponse(unknownResponse, 404);
    assert.equal(parseJson(unknownResponse).error, 'API_ROUTE_NOT_FOUND');

    const legacyRoutes = ['/md', '/ai/catalog', '/ai/page-context', '/ai/code', '/chunks/search']
      .map((suffix) => `/api${suffix}`);
    for (const route of legacyRoutes) {
      const response = await app.inject(route);
      assertApiResponse(response, 404);
      assert.equal(parseJson(response).error, 'API_ROUTE_NOT_FOUND');
    }

    console.log('[test:api] public API contract checks passed');
  } finally {
    await app.close();
  }
}

main()
  .catch((error) => {
    console.error('[test:api] public API contract checks failed');
    console.error(error);
    process.exitCode = 1;
  })
  .finally(() => {
    fs.rmSync(runtimeDir, { recursive: true, force: true });
  });
