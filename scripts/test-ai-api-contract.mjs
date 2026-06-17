import assert from 'node:assert/strict';
import { createApp } from '../packages/rbook-server/dist/app.js';

const staticDir = process.env.RBOOK_TEST_STATIC_DIR || '/tmp/rbook-runtime-check/dist';
const forbiddenAbsoluteLinkFields = new Set(['href', 'codeHref']);

function parseJson(response) {
  assert.equal(
    response.headers['content-type']?.startsWith('application/json'),
    true,
    `expected JSON response, got ${response.headers['content-type']}`
  );
  return JSON.parse(response.body);
}

function assertNoForbiddenFields(value, path = '$') {
  if (!value || typeof value !== 'object') return;

  if (Array.isArray(value)) {
    value.forEach((item, index) => assertNoForbiddenFields(item, `${path}[${index}]`));
    return;
  }

  for (const [key, child] of Object.entries(value)) {
    assert.equal(
      forbiddenAbsoluteLinkFields.has(key),
      false,
      `AI API should not expose ${path}.${key}`
    );
    assertNoForbiddenFields(child, `${path}.${key}`);
  }
}

function assertNoLoopbackUrl(value) {
  assert.equal(
    JSON.stringify(value).includes('127.0.0.1'),
    false,
    'AI API response should not include 127.0.0.1'
  );
}

function assertRelativeUrl(url, label) {
  assert.equal(typeof url, 'string', `${label} should be a string`);
  assert.equal(url.startsWith('/'), true, `${label} should be a root-relative URL`);
  assert.equal(/^https?:\/\//.test(url), false, `${label} should not be absolute`);
}

function findArticle(catalog, path) {
  return catalog.articles.find((article) => article.path === path);
}

async function main() {
  const app = await createApp({ logger: false, staticDir });

  try {
    const catalogResponse = await app.inject('/api/ai/catalog');
    assert.equal(catalogResponse.statusCode, 200, 'catalog should return HTTP 200');
    const catalog = parseJson(catalogResponse);
    assert.equal(catalog.scope, 'visible', 'catalog default scope should be visible');
    assert.ok(catalog.total > 0, 'catalog should contain articles');
    assertNoForbiddenFields(catalog);
    assertNoLoopbackUrl(catalog);

    const randomArticle = findArticle(catalog, 'utils/random/index.md');
    assert.ok(randomArticle, 'catalog should include utils/random/index.md');
    assertRelativeUrl(randomArticle.url, 'article.url');
    assert.equal(randomArticle.citation.path, randomArticle.path, 'citation.path should match article.path');
    assertRelativeUrl(randomArticle.citation.url, 'citation.url');
    assert.ok(Array.isArray(randomArticle.codeTemplates), 'article.codeTemplates should be an array');
    assert.ok(randomArticle.codeTemplates.length > 0, 'random article should expose a code template');
    assertRelativeUrl(randomArticle.codeTemplates[0].codeUrl, 'codeTemplates[0].codeUrl');
    assert.equal(randomArticle.codeTemplates[0].code, '/code/utils/random.cpp');

    const contextResponse = await app.inject('/api/ai/page-context?path=utils/random/index.md&includeCode=true');
    assert.equal(contextResponse.statusCode, 200, 'page-context should return HTTP 200');
    const context = parseJson(contextResponse);
    assertNoForbiddenFields(context);
    assertNoLoopbackUrl(context);
    assert.equal(context.article.path, 'utils/random/index.md');
    assertRelativeUrl(context.article.url, 'context.article.url');
    assertRelativeUrl(context.article.citation.url, 'context.article.citation.url');
    assert.ok(context.article.markdown.includes('@include-code(/code/utils/random.cpp, cpp)'));
    assert.ok(context.codeTemplates.length > 0, 'page-context should include codeTemplates');
    assert.ok(context.codeTemplates[0].content.includes('mt19937'));
    assertRelativeUrl(context.codeTemplates[0].codeUrl, 'context.codeTemplates[0].codeUrl');
    assert.ok(context.includedCode.length > 0, 'page-context should include include-code entries');
    assert.ok(context.includedCode[0].content.includes('mt19937'));
    assertRelativeUrl(context.includedCode[0].codeUrl, 'context.includedCode[0].codeUrl');

    const codeResponse = await app.inject('/api/ai/code?path=/code/utils/random.cpp');
    assert.equal(codeResponse.statusCode, 200, 'ai code should return HTTP 200');
    const code = parseJson(codeResponse);
    assertNoForbiddenFields(code);
    assertNoLoopbackUrl(code);
    assert.equal(code.path, '/code/utils/random.cpp');
    assertRelativeUrl(code.url, 'code.url');
    assert.equal(code.language, 'cpp');
    assert.ok(code.content.includes('mt19937'));

    const badCodeResponse = await app.inject('/api/ai/code?path=/../package.json');
    assert.equal(badCodeResponse.statusCode, 400, 'invalid code path should be rejected');
    const badCode = parseJson(badCodeResponse);
    assert.match(badCode.error, /invalid code path/);

    console.log('[test:api] AI API contract checks passed');
  } finally {
    await app.close();
  }
}

main().catch((error) => {
  console.error('[test:api] AI API contract checks failed');
  console.error(error);
  process.exit(1);
});
