import assert from 'node:assert/strict';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const clientPath = path.join(rootDir, 'scripts/rbook.py');
const skillClientPath = path.join(rootDir, '.agents/skills/rbook-http/scripts/rbook.py');

function runPython(args) {
  return spawnSync('python3', args, {
    cwd: rootDir,
    encoding: 'utf8'
  });
}

function runClient(args, responses = {}, expectedParams = {}) {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)

responses = json.loads(${JSON.stringify(JSON.stringify(responses))})
expected_params = json.loads(${JSON.stringify(JSON.stringify(expectedParams))})

def fake_request(baseurl, path, params=None):
    if path in expected_params and params != expected_params[path]:
        raise AssertionError(f"unexpected params for {path}: {params!r}")
    return responses[path]

client.request_json = fake_request
raise SystemExit(client.main(json.loads(${JSON.stringify(JSON.stringify(args))})))
`;
  return runPython(['-c', program]);
}

function runClientError(args, code, message) {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)

def fail_request(baseurl, path, params=None):
    raise client.RbookClientError(${JSON.stringify(code)}, ${JSON.stringify(message)})

client.request_json = fail_request
raise SystemExit(client.main(json.loads(${JSON.stringify(JSON.stringify(args))})))
`;
  return runPython(['-c', program]);
}

test('find requires every word and ranks id or title before tags and description', () => {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)

pages = [
    {"id": "other", "title": "Other", "description": "kmp 字符串", "tags": []},
    {"id": "kmp-algo", "title": "KMP", "description": "字符串匹配", "tags": ["字符串"]},
    {"id": "kmp-only", "title": "KMP only", "description": "prefix", "tags": []},
]
print(json.dumps(client.find_pages(pages, "kmp 字符串", 20), ensure_ascii=False))
`;
  const result = runPython(['-c', program]);
  assert.equal(result.status, 0, result.stderr);
  const payload = JSON.parse(result.stdout);
  assert.equal(payload.total, 2);
  assert.deepEqual(payload.items.map((item) => item.id), ['kmp-algo', 'other']);
});

test('find applies its result limit after counting all matches', () => {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)
pages = [{"id": f"page-{index}", "title": "Graph", "description": "", "tags": []} for index in range(25)]
print(json.dumps(client.find_pages(pages, "graph", 20)))
`;
  const result = runPython(['-c', program]);
  assert.equal(result.status, 0, result.stderr);
  const payload = JSON.parse(result.stdout);
  assert.equal(payload.total, 25);
  assert.equal(payload.items.length, 20);
});

test('the skill entry delegates to the canonical client', () => {
  const canonical = runPython([clientPath, '--help']);
  const skill = runPython([skillClientPath, '--help']);
  assert.equal(canonical.status, 0, canonical.stderr);
  assert.equal(skill.status, 0, skill.stderr);
  assert.equal(skill.stdout, canonical.stdout);
  assert.match(skill.stdout, /\bfind\b/);
});

test('catalog defaults to numbered TSV with stable single-line fields', () => {
  const response = {
    generatedAt: '2026-07-26T00:00:00.000Z',
    total: 2,
    items: [
      {
        id: 'kmp',
        title: 'KMP',
        description: '第一行\n第二行\t补充',
        tags: ['字符串', '匹配算法'],
        path: 'string/kmp/index.md'
      },
      {
        id: 'binary-search',
        title: '二分查找',
        description: '有序集   上的查找',
        tags: ['二分']
      }
    ]
  };
  const result = runClient(
    ['catalog'],
    {'/api/catalog': response},
    {'/api/catalog': {compact: 'true'}}
  );

  assert.equal(result.status, 0, result.stderr);
  assert.equal(
    result.stdout,
    '#\tid\ttitle\tdescription\ttags\n' +
      '1\tkmp\tKMP\t第一行 第二行 补充\t字符串,匹配算法\n' +
      '2\tbinary-search\t二分查找\t有序集 上的查找\t二分\n'
  );
});

test('article list JSON is pretty printed and keeps only compact fields', () => {
  const response = {
    generatedAt: '2026-07-26T00:00:00.000Z',
    total: 1,
    items: [{
      id: 'kmp',
      title: 'KMP',
      description: '字符串匹配',
      tags: ['字符串'],
      path: 'string/kmp/index.md'
    }]
  };
  const result = runClient(['catalog', '--json'], {'/api/catalog': response});

  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), {
    total: 1,
    items: [{id: 'kmp', title: 'KMP', description: '字符串匹配', tags: ['字符串']}]
  });
  assert.match(result.stdout, /^\{\n  "total": 1,\n  "items": \[/);
});

test('paged article TSV numbering restarts at one', () => {
  const response = {
    total: 100,
    items: [{id: 'page-51', title: '第五十一篇', description: '', tags: ['分页']}]
  };
  const result = runClient(
    ['pages', '--offset', '50', '--limit', '1'],
    {'/api/pages': response},
    {'/api/pages': {limit: '1', offset: '50'}}
  );

  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, /^#\tid\ttitle\tdescription\ttags\n1\tpage-51\t第五十一篇\t\t分页\n$/);
});

test('code lists map description to title and omit description', () => {
  const response = {
    total: 1,
    items: [{
      id: 'binary-search',
      title: '代码模板标题',
      description: '二分查找标准模板',
      language: 'cpp',
      tags: ['二分'],
      path: 'base/binary_search.cpp'
    }]
  };
  const tsv = runClient(['codes'], {'/api/codes': response});
  const json = runClient(['codes', '--json'], {'/api/codes': response});

  assert.equal(tsv.status, 0, tsv.stderr);
  assert.equal(tsv.stdout, '#\tid\ttitle\tlanguage\ttags\n1\tbinary-search\t二分查找标准模板\tcpp\t二分\n');
  assert.deepEqual(JSON.parse(json.stdout), {
    total: 1,
    items: [{id: 'binary-search', title: '二分查找标准模板', language: 'cpp', tags: ['二分']}]
  });
});

test('code lists support an ASCII table format', () => {
  const response = {
    total: 1,
    items: [{
      id: 'binary-search',
      description: '二分查找标准模板',
      language: 'cpp',
      tags: ['二分']
    }]
  };
  const result = runClient(['codes', '--table'], {'/api/codes': response});

  assert.equal(result.status, 0, result.stderr);
  assert.equal(
    result.stdout,
    '+---+---------------+------------------+----------+------+\n' +
      '| # | id            | title            | language | tags |\n' +
      '+---+---------------+------------------+----------+------+\n' +
      '| 1 | binary-search | 二分查找标准模板 | cpp      | 二分 |\n' +
      '+---+---------------+------------------+----------+------+\n'
  );
});

test('page and code details default to raw content', () => {
  const page = {
    id: 'kmp',
    title: 'KMP',
    markdown: '# KMP\n\n正文\n',
    html: '<h1>KMP</h1>'
  };
  const code = {
    id: 'binary-search',
    description: '二分查找标准模板',
    language: 'cpp',
    content: 'int binary_search() {\n  return 0;\n}\n'
  };
  const pageResult = runClient(
    ['pages', '--id', 'kmp'],
    {'/api/pages': page},
    {'/api/pages': {id: 'kmp'}}
  );
  const codeResult = runClient(
    ['codes', '--id', 'binary-search'],
    {'/api/codes': code},
    {'/api/codes': {id: 'binary-search', includeContent: 'true'}}
  );

  assert.equal(pageResult.status, 0, pageResult.stderr);
  assert.equal(pageResult.stdout, page.markdown);
  assert.equal(codeResult.status, 0, codeResult.stderr);
  assert.equal(codeResult.stdout, code.content);
});

test('article detail JSON omits derived renderings while code detail stays complete', () => {
  const page = {
    id: 'kmp',
    title: 'KMP',
    description: '字符串匹配',
    path: 'string/kmp/index.md',
    url: '/string/kmp/index.html',
    tags: ['字符串'],
    categories: ['字符串算法'],
    frontMatter: {id: 'kmp', code_template: ['kmp']},
    headings: [{level: 2, title: '前缀函数'}],
    navTrail: ['字符串', 'KMP'],
    markdown: '# KMP',
    html: '<h1>KMP</h1>',
    text: 'KMP',
    excerpt: '字符串匹配',
    visible: true,
    source: 'chapters'
  };
  const code = {
    id: 'binary-search',
    description: '二分查找标准模板',
    language: 'cpp',
    articles: [{id: 'binary-search-article'}],
    content: 'int main() {}'
  };
  const pageResult = runClient(['pages', '--id', 'kmp', '--json'], {'/api/pages': page});
  const codeResult = runClient(['codes', '--id', 'binary-search', '--json'], {'/api/codes': code});

  assert.deepEqual(JSON.parse(pageResult.stdout), {
    id: 'kmp',
    title: 'KMP',
    description: '字符串匹配',
    path: 'string/kmp/index.md',
    url: '/string/kmp/index.html',
    tags: ['字符串'],
    categories: ['字符串算法'],
    frontMatter: {id: 'kmp', code_template: ['kmp']},
    headings: [{level: 2, title: '前缀函数'}],
    navTrail: ['字符串', 'KMP'],
    markdown: '# KMP'
  });
  assert.deepEqual(JSON.parse(codeResult.stdout), code);
});

test('health, site and tags have concise human-readable output', () => {
  const health = runClient(['health'], {
    '/api/health': {
      ok: true,
      generatedAt: '2026-07-26T00:00:00.000Z',
      stats: {pages: 432, codes: 189, errors: 0}
    }
  });
  const site = runClient(['site'], {
    '/api/site': {
      site: {
        title: '我的算法书',
        author: 'rainboy',
        description: '这是一本关于算法的书',
        github_repository: 'https://example.com/rbook'
      },
      stats: {pages: 432, codes: 189, errors: 0},
      generatedAt: '2026-07-26T00:00:00.000Z'
    }
  });
  const tags = runClient(['tags'], {
    '/api/tags': {
      generatedAt: '2026-07-26T00:00:00.000Z',
      articleTags: [{tag: '图论', count: 20}],
      codeTags: [{tag: '图', count: 12}]
    }
  });

  assert.equal(
    health.stdout,
    'key\tvalue\nok\ttrue\ngeneratedAt\t2026-07-26T00:00:00.000Z\npages\t432\ncodes\t189\nerrors\t0\n'
  );
  assert.match(site.stdout, /^key\tvalue\ntitle\t我的算法书\nauthor\trainboy\n/);
  assert.equal(tags.stdout, '#\ttype\ttag\tcount\n1\tarticle\t图论\t20\n2\tcode\t图\t12\n');
});

test('errors follow text and JSON output modes', () => {
  const textResult = runClientError(
    ['pages', '--id', 'missing'],
    'PAGE_NOT_FOUND',
    "page with id 'missing' not found"
  );
  const jsonResult = runClientError(
    ['pages', '--id', 'missing', '--json'],
    'PAGE_NOT_FOUND',
    "page with id 'missing' not found"
  );

  assert.equal(textResult.status, 1);
  assert.equal(textResult.stderr, "PAGE_NOT_FOUND: page with id 'missing' not found\n");
  assert.equal(jsonResult.status, 1);
  assert.equal(jsonResult.stderr.trim().split('\n').length, 1);
  assert.deepEqual(JSON.parse(jsonResult.stderr), {
    error: 'PAGE_NOT_FOUND',
    message: "page with id 'missing' not found"
  });
});

test('detail IDs reject list-only filters', () => {
  const textResult = runClient(['pages', '--id', 'kmp', '--tag', '字符串']);
  const jsonResult = runClient(['codes', '--id', 'binary-search', '--limit', '1', '--json']);
  const tableResult = runClient(['codes', '--id', 'binary-search', '--table']);

  assert.equal(textResult.status, 2);
  assert.equal(textResult.stderr, 'ARGUMENT_ERROR: --id cannot be used with --tag\n');
  assert.equal(jsonResult.status, 2);
  assert.deepEqual(JSON.parse(jsonResult.stderr), {
    error: 'ARGUMENT_ERROR',
    message: '--id cannot be used with --limit'
  });
  assert.equal(tableResult.status, 2);
  assert.equal(tableResult.stderr, 'ARGUMENT_ERROR: --id cannot be used with --table\n');
});

test('removed commands and options are no longer accepted', () => {
  const codeCommand = runPython([clientPath, 'code', 'binary-search']);
  const compactOption = runPython([clientPath, 'catalog', '--compact']);
  const contentOption = runPython([clientPath, 'codes', '--content']);
  const oldGlobalOption = runPython([clientPath, '--compact-json', 'catalog']);

  assert.equal(codeCommand.status, 2);
  assert.equal(compactOption.status, 2);
  assert.equal(contentOption.status, 2);
  assert.equal(oldGlobalOption.status, 2);
});
