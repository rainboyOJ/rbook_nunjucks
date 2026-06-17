import path from 'path';
import fs from 'fs';
import yaml from 'js-yaml';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import Markdown from '@rbook/markdown';
import { bookDir, codeTemplateDir, configPath, distDir } from '@rbook/core/paths';
import {
  getIndexPayload,
  getPage,
  rebuildIndex,
  searchChunks,
  searchPages
} from '@rbook/search';
import { loadPageDocument } from '@rbook/search/markdownText';
import type { FastifyReply, FastifyRequest } from 'fastify';

interface CreateAppOptions {
  logger?: boolean;
  staticDir?: string;
}

type QueryParams = Record<string, string | undefined>;

interface ApiEndpointDoc {
  method: 'GET' | 'POST';
  path: string;
  description: string;
  query: string;
  example: string;
  note?: string;
}

const apiEndpointDocs: ApiEndpointDoc[] = [
  {
    method: 'GET',
    path: '/api/health',
    description: '服务健康检查，返回索引生成时间和页面/chunk 统计。',
    query: '-',
    example: 'curl "$BASE_URL/api/health"'
  },
  {
    method: 'GET',
    path: '/api/site',
    description: '返回电子书站点元信息、统计信息和索引生成时间。',
    query: '-',
    example: 'curl "$BASE_URL/api/site"'
  },
  {
    method: 'GET',
    path: '/api/toc',
    description: '返回电子书目录树，适合定位主题在书中的位置。',
    query: '-',
    example: 'curl "$BASE_URL/api/toc"'
  },
  {
    method: 'GET',
    path: '/api/nav',
    description: '返回和 /api/toc 相同的导航树，保留给导航语义调用。',
    query: '-',
    example: 'curl "$BASE_URL/api/nav"'
  },
  {
    method: 'GET',
    path: '/api/pages',
    description: '返回页面元数据列表。',
    query: 'visible=true 可选，只返回目录中可见页面',
    example: 'curl "$BASE_URL/api/pages?visible=true"'
  },
  {
    method: 'GET',
    path: '/api/page',
    description: '返回单页详情，包括 front matter、headings、markdown、html、text 和 chunks。',
    query: 'path 必填，例如 dynamic_programming/数位DP/index.md',
    example: 'curl "$BASE_URL/api/page?path=dynamic_programming/%E6%95%B0%E4%BD%8DDP/index.md"'
  },
  {
    method: 'GET',
    path: '/api/search',
    description: '页面级全文搜索，适合先找到相关页面。',
    query: 'q 必填；limit 可选，最大 50',
    example: 'curl -G --data-urlencode "q=数位DP" --data-urlencode "limit=5" "$BASE_URL/api/search"'
  },
  {
    method: 'GET',
    path: '/api/chunks/search',
    description: 'chunk 级搜索，适合 agent 获取更聚焦的上下文。',
    query: 'q 必填；limit、textLength、includeText 可选',
    example: 'curl -G --data-urlencode "q=数位DP 状态 记忆化" --data-urlencode "limit=8" --data-urlencode "textLength=900" "$BASE_URL/api/chunks/search"'
  },
  {
    method: 'GET',
    path: '/api/ai/catalog',
    description: '返回 AI 友好的正式文章目录，包含文章链接、摘要、标签和 code_template 元数据。',
    query: 'scope=visible|all 可选，默认 visible',
    example: 'curl "$BASE_URL/api/ai/catalog"'
  },
  {
    method: 'GET',
    path: '/api/ai/page-context',
    description: '返回 AI 解题所需的单页上下文，可包含完整文章、引用信息、模板代码和正文 include-code 代码。',
    query: 'path 必填；includeCode=true 可选；includeHtml=true 可选',
    example: 'curl "$BASE_URL/api/ai/page-context?path=graph/bcc/index.md&includeCode=true"'
  },
  {
    method: 'GET',
    path: '/api/ai/code',
    description: '按 /code/... 路径读取模板代码内容，只允许访问本项目 book/code 下的文件。',
    query: 'path 必填，例如 /code/graph/v-bcc.cpp',
    example: 'curl "$BASE_URL/api/ai/code?path=/code/graph/v-bcc.cpp"'
  },
  {
    method: 'POST',
    path: '/api/admin/reindex',
    description: '重建搜索索引。',
    query: '-',
    example: 'curl -X POST -H "Authorization: Bearer $RBOOK_ADMIN_TOKEN" "$BASE_URL/api/admin/reindex"',
    note: '生产环境设置 RBOOK_ADMIN_TOKEN 后，需要 Bearer token 或 x-rbook-token。'
  }
];

function getQuery(request: FastifyRequest): QueryParams {
  return request.query as QueryParams;
}

function parseLimit(value: string | number | undefined, fallback = 10) {
  const limit = Number(value || fallback);
  if (!Number.isFinite(limit) || limit <= 0) return fallback;
  return Math.min(Math.floor(limit), 50);
}

function loadBookConfig() {
  return (yaml.load(fs.readFileSync(configPath, 'utf8')) || {}) as Record<string, unknown>;
}

function stripHtml(html: string) {
  return html
    .replace(/<script[\s\S]*?<\/script>/gi, ' ')
    .replace(/<style[\s\S]*?<\/style>/gi, ' ')
    .replace(/<[^>]+>/g, ' ')
    .replace(/&nbsp;/g, ' ')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&amp;/g, '&')
    .replace(/\s+/g, ' ')
    .trim();
}

function escapeHtml(value: string) {
  return value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}

function getBaseUrl(request: FastifyRequest) {
  const protocolHeader = request.headers['x-forwarded-proto'];
  const protocol = Array.isArray(protocolHeader)
    ? protocolHeader[0]
    : protocolHeader || request.protocol;
  return `${protocol}://${request.headers.host || '127.0.0.1:3000'}`;
}

function buildHref(baseUrl: string, url: string | null | undefined) {
  if (!url) return null;
  return new URL(url, baseUrl.endsWith('/') ? baseUrl : `${baseUrl}/`).toString();
}

function asRecord(value: unknown): Record<string, any> {
  return value && typeof value === 'object' && !Array.isArray(value)
    ? value as Record<string, any>
    : {};
}

function asStringArray(value: unknown) {
  return Array.isArray(value)
    ? value.filter((item) => typeof item === 'string')
    : [];
}

function getPageDescription(page: any) {
  const frontMatter = asRecord(page.frontMatter);
  return String(frontMatter.description || frontMatter.desc || page.excerpt || '').trim();
}

function createCitation(page: any, baseUrl: string) {
  return {
    title: page.title,
    path: page.path,
    url: page.url,
    href: buildHref(baseUrl, page.url)
  };
}

function normalizeCodeUrl(codePath: string | undefined) {
  if (!codePath || typeof codePath !== 'string') return null;
  const trimmed = codePath.trim();
  if (!trimmed) return null;

  if (trimmed.startsWith('/code/')) return trimmed.replace(/\\/g, '/');
  if (trimmed.startsWith('code/')) return `/${trimmed.replace(/\\/g, '/')}`;
  return null;
}

function resolveCodeFile(codePath: string | undefined) {
  const codeUrl = normalizeCodeUrl(codePath);
  if (!codeUrl) return null;

  const relativePath = codeUrl.replace(/^\/code\/?/, '');
  const absolutePath = path.resolve(codeTemplateDir, relativePath);
  const codeRoot = path.resolve(codeTemplateDir);
  const relativeToRoot = path.relative(codeRoot, absolutePath);

  if (relativeToRoot.startsWith('..') || path.isAbsolute(relativeToRoot)) return null;

  return {
    path: codeUrl,
    url: codeUrl,
    absolutePath
  };
}

function getLanguageFromPath(filePath: string) {
  const ext = path.extname(filePath).replace(/^\./, '');
  return ext || 'text';
}

function readCodePayload(codePath: string | undefined, baseUrl: string, includeContent = false) {
  const resolved = resolveCodeFile(codePath);
  if (!resolved) return null;

  const payload: Record<string, unknown> = {
    path: resolved.path,
    url: resolved.url,
    href: buildHref(baseUrl, resolved.url),
    language: getLanguageFromPath(resolved.path)
  };

  if (includeContent) {
    if (fs.existsSync(resolved.absolutePath)) {
      payload.content = fs.readFileSync(resolved.absolutePath, 'utf8');
    } else {
      payload.error = `code file not found: ${resolved.path}`;
    }
  }

  return payload;
}

function getCodeTemplates(page: any, baseUrl: string, includeContent = false) {
  const frontMatter = asRecord(page.frontMatter);
  const templates = Array.isArray(frontMatter.code_template) ? frontMatter.code_template : [];

  return templates
    .map((template: any) => {
      const code = readCodePayload(template?.code, baseUrl, includeContent);
      if (!code) return null;
      return {
        source: 'frontMatter',
        title: template.title || '',
        desc: template.desc || '',
        tags: asStringArray(template.tags),
        ...template,
        code: code.path,
        codeUrl: code.url,
        codeHref: code.href,
        language: code.language,
        ...(includeContent ? { content: code.content } : {})
      };
    })
    .filter(Boolean);
}

const includeCodeRegex = /^@include-code\(([^,)]+)(?:,\s*([^)]+))?\s*\)/gm;

function resolveIncludedCode(rawCodePath: string, pagePath: string) {
  const codeUrl = normalizeCodeUrl(rawCodePath);
  if (codeUrl) {
    const resolved = resolveCodeFile(codeUrl);
    return resolved ? { ...resolved, url: codeUrl } : null;
  }

  const fullPath = path.resolve(bookDir, path.dirname(pagePath), rawCodePath.trim());
  const bookRoot = path.resolve(bookDir);
  const relativeToBook = path.relative(bookRoot, fullPath);
  if (relativeToBook.startsWith('..') || path.isAbsolute(relativeToBook)) return null;

  return {
    path: rawCodePath.trim(),
    url: null,
    absolutePath: fullPath
  };
}

function getIncludedCode(pagePath: string, markdown: string, baseUrl: string, includeContent = false) {
  const included = [];
  const seen = new Set<string>();

  for (const match of markdown.matchAll(includeCodeRegex)) {
    const rawCodePath = match[1].trim();
    const language = (match[2] || '').trim();
    const resolved = resolveIncludedCode(rawCodePath, pagePath);
    if (!resolved || seen.has(resolved.absolutePath)) continue;
    seen.add(resolved.absolutePath);

    const payload: Record<string, unknown> = {
      source: 'include-code',
      path: resolved.path,
      code: rawCodePath,
      codeUrl: resolved.url,
      codeHref: resolved.url ? buildHref(baseUrl, resolved.url) : null,
      language: language || getLanguageFromPath(rawCodePath)
    };

    if (includeContent) {
      if (fs.existsSync(resolved.absolutePath)) {
        payload.content = fs.readFileSync(resolved.absolutePath, 'utf8');
      } else {
        payload.error = `code file not found: ${rawCodePath}`;
      }
    }

    included.push(payload);
  }

  return included;
}

function createAiCatalogItem(page: any, baseUrl: string) {
  const frontMatter = asRecord(page.frontMatter);
  return {
    path: page.path,
    url: page.url,
    href: buildHref(baseUrl, page.url),
    title: page.title,
    description: getPageDescription(page),
    excerpt: page.excerpt || '',
    tags: asStringArray(frontMatter.tags),
    categories: asStringArray(frontMatter.categories),
    headings: page.headings || [],
    navTrail: page.navTrail || [],
    visible: page.visible,
    source: page.source,
    codeTemplates: getCodeTemplates(page, baseUrl, false),
    citation: createCitation(page, baseUrl)
  };
}

function renderApiDocsPage(baseUrl: string) {
  const endpointHtml = apiEndpointDocs.map((endpoint) => `
    <section class="endpoint">
      <div class="endpoint-title">
        <span class="method ${endpoint.method.toLowerCase()}">${endpoint.method}</span>
        <code>${escapeHtml(endpoint.path)}</code>
      </div>
      <p>${escapeHtml(endpoint.description)}</p>
      <dl>
        <dt>Query</dt>
        <dd><code>${escapeHtml(endpoint.query)}</code></dd>
        <dt>Example</dt>
        <dd><pre><code>${escapeHtml(endpoint.example.replace(/\$BASE_URL/g, baseUrl))}</code></pre></dd>
        ${endpoint.note ? `<dt>Note</dt><dd>${escapeHtml(endpoint.note)}</dd>` : ''}
      </dl>
    </section>
  `).join('');

  return `<!doctype html>
<html lang="zh">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>rbook API 文档</title>
  <style>
    :root {
      color-scheme: light dark;
      --bg: #f7f7f4;
      --panel: #ffffff;
      --text: #1f2933;
      --muted: #5d6978;
      --border: #d9dee5;
      --code: #eef2f6;
      --accent: #16745f;
      --post: #7a4d12;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #151719;
        --panel: #1f2328;
        --text: #e6edf3;
        --muted: #a6b0bd;
        --border: #3a424c;
        --code: #2b3138;
      }
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      background: var(--bg);
      color: var(--text);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      line-height: 1.65;
    }
    main {
      width: min(980px, calc(100% - 32px));
      margin: 0 auto;
      padding: 42px 0 64px;
    }
    header {
      margin-bottom: 26px;
      border-bottom: 1px solid var(--border);
      padding-bottom: 20px;
    }
    h1 {
      margin: 0 0 8px;
      font-size: clamp(28px, 5vw, 42px);
      line-height: 1.2;
    }
    h2 {
      margin: 30px 0 12px;
      font-size: 22px;
    }
    p { margin: 0 0 12px; }
    .muted { color: var(--muted); }
    .base-url {
      display: inline-block;
      margin-top: 8px;
      padding: 8px 10px;
      border: 1px solid var(--border);
      border-radius: 6px;
      background: var(--panel);
    }
    .endpoint {
      background: var(--panel);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 18px;
      margin: 14px 0;
    }
    .endpoint-title {
      display: flex;
      gap: 10px;
      align-items: center;
      flex-wrap: wrap;
      margin-bottom: 8px;
    }
    .method {
      min-width: 54px;
      text-align: center;
      border-radius: 999px;
      padding: 3px 9px;
      color: #fff;
      background: var(--accent);
      font-size: 13px;
      font-weight: 700;
    }
    .method.post { background: var(--post); }
    code, pre {
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
      font-size: 0.93em;
    }
    code {
      background: var(--code);
      border-radius: 4px;
      padding: 2px 5px;
    }
    pre {
      overflow-x: auto;
      margin: 0;
      padding: 10px;
      background: var(--code);
      border-radius: 6px;
    }
    pre code {
      padding: 0;
      background: transparent;
    }
    dl {
      display: grid;
      grid-template-columns: 90px minmax(0, 1fr);
      gap: 8px 12px;
      margin: 10px 0 0;
    }
    dt {
      color: var(--muted);
      font-weight: 700;
    }
    dd { margin: 0; min-width: 0; }
    ul { padding-left: 22px; }
    a { color: var(--accent); }
    @media (max-width: 640px) {
      main { width: min(100% - 20px, 980px); padding-top: 26px; }
      .endpoint { padding: 14px; }
      dl { grid-template-columns: 1fr; }
    }
  </style>
</head>
<body>
  <main>
    <header>
      <h1>rbook API 文档</h1>
      <p class="muted">这个服务只提供算法电子书内容检索和读取 API，不提供 chat，也不在站点内嵌入 AI。</p>
      <div class="base-url"><strong>BASE_URL</strong> <code>${escapeHtml(baseUrl)}</code></div>
    </header>

    <section>
      <h2>推荐调用流程</h2>
      <ul>
        <li>AI 解题或生成题解时，先用 <code>/api/ai/catalog</code> 获取正式文章和模板目录。</li>
        <li>确定页面后，用 <code>/api/ai/page-context?path=...&amp;includeCode=true</code> 获取完整文章和模板代码。</li>
        <li>只需要模板代码时，用 <code>/api/ai/code?path=/code/...</code> 读取代码内容。</li>
        <li>普通搜索仍可使用 <code>/api/chunks/search</code> 或 <code>/api/search</code>。</li>
        <li>本项目不提供题目数据查询；题目数据已经拆到独立服务。</li>
      </ul>
    </section>

    <section>
      <h2>Endpoints</h2>
      ${endpointHtml}
    </section>
  </main>
</body>
</html>`;
}

function buildTocNode(item: any, pagesByPath: Map<string, any>, basePath = '', trail: string[] = []) {
  if (!item || !item.title) return null;

  const type = item.type || 'page';
  const nextTrail = type === 'info' ? trail : [...trail, item.title].filter(Boolean);
  const rawPath = item.path ? path.posix.join(basePath, item.path) : '';
  const sections = Array.isArray(item.sections)
    ? item.sections
        .map((section: any) => buildTocNode(section, pagesByPath, rawPath, nextTrail))
        .filter(Boolean)
    : [];

  const candidates = [
    rawPath,
    `${rawPath}.md`,
    path.posix.join(rawPath, 'index.md')
  ].filter(Boolean);
  const page = candidates.map((candidate) => pagesByPath.get(candidate)).find(Boolean);

  return {
    title: item.title,
    type,
    path: page?.path || null,
    url: page?.url || null,
    navTrail: page?.navTrail || nextTrail,
    visible: page?.visible ?? type !== 'info',
    children: sections
  };
}

function buildToc(index: any) {
  const pagesByPath = new Map<string, any>(index.pages.map((page: any) => [page.path, page]));
  const config = loadBookConfig();
  const chapters = Array.isArray(config.chapters) ? config.chapters : [];
  return chapters
    .map((chapter: any) => buildTocNode(chapter, pagesByPath))
    .filter(Boolean);
}

function createPagePayload(page: any) {
  const fullPath = path.join(bookDir, page.path);
  const document = loadPageDocument(page);
  const renderer = new Markdown(fullPath);
  const rendered = renderer.toJSON();

  return {
    ...page,
    title: document.title,
    url: document.url,
    frontMatter: document.frontMatter,
    headings: document.headings,
    excerpt: document.excerpt,
    markdown: rendered.md_content || page.markdown || '',
    html: rendered.html_content || '',
    text: document.text || stripHtml(rendered.html_content || ''),
    chunks: document.chunks
  };
}

function requireAdminToken(request: FastifyRequest, reply: FastifyReply) {
  const expected = process.env.RBOOK_ADMIN_TOKEN;
  if (!expected) return true;

  const authorization = request.headers.authorization || '';
  const token = authorization.startsWith('Bearer ')
    ? authorization.slice('Bearer '.length)
    : request.headers['x-rbook-token'];

  if (token === expected) return true;

  reply.code(401).send({ error: 'unauthorized' });
  return false;
}

export async function createApp(options: CreateAppOptions = {}) {
  const app = fastify({
    logger: options.logger ?? true
  });

  app.get('/api', async (request, reply) => {
    reply.type('text/html; charset=utf-8');
    return renderApiDocsPage(getBaseUrl(request));
  });

  app.get('/api/health', async () => {
    const index = getIndexPayload();
    return {
      ok: true,
      generatedAt: index.generatedAt,
      stats: index.stats
    };
  });

  app.get('/api/site', async () => {
    const index = getIndexPayload();
    return {
      site: index.site,
      stats: index.stats,
      generatedAt: index.generatedAt
    };
  });

  app.get('/api/toc', async () => {
    const index = getIndexPayload();
    return {
      generatedAt: index.generatedAt,
      toc: buildToc(index)
    };
  });

  app.get('/api/nav', async () => {
    const index = getIndexPayload();
    return {
      generatedAt: index.generatedAt,
      nav: buildToc(index)
    };
  });

  app.get('/api/pages', async (request) => {
    const index = getIndexPayload();
    const query = getQuery(request);
    const visibleOnly = query.visible === 'true';
    const pages = visibleOnly ? index.pages.filter((page: any) => page.visible) : index.pages;
    return {
      total: pages.length,
      pages
    };
  });

  app.get('/api/page', async (request, reply) => {
    const query = getQuery(request);
    const pagePath = query.path;
    if (!pagePath) {
      reply.code(400);
      return { error: 'missing query parameter: path' };
    }

    const page = getPage(pagePath);
    if (!page) {
      reply.code(404);
      return { error: 'page not found' };
    }

    return createPagePayload(page);
  });

  app.get('/api/search', async (request, reply) => {
    const query = getQuery(request);
    const searchQuery = query.q || query.query;
    if (!searchQuery) {
      reply.code(400);
      return { error: 'missing query parameter: q' };
    }

    return searchPages(searchQuery, {
      limit: parseLimit(query.limit)
    });
  });

  app.get('/api/chunks/search', async (request, reply) => {
    const query = getQuery(request);
    const searchQuery = query.q || query.query;
    if (!searchQuery) {
      reply.code(400);
      return { error: 'missing query parameter: q' };
    }

    return searchChunks(searchQuery, {
      limit: parseLimit(query.limit),
      textLength: Number(query.textLength || 900),
      includeText: query.includeText !== 'false'
    });
  });

  app.get('/api/ai/catalog', async (request) => {
    const index = getIndexPayload();
    const query = getQuery(request);
    const scope = query.scope === 'all' ? 'all' : 'visible';
    const baseUrl = getBaseUrl(request);
    const pages = scope === 'all'
      ? index.pages
      : index.pages.filter((page: any) => page.visible);

    return {
      generatedAt: index.generatedAt,
      scope,
      total: pages.length,
      articles: pages.map((page: any) => createAiCatalogItem(page, baseUrl))
    };
  });

  app.get('/api/ai/page-context', async (request, reply) => {
    const query = getQuery(request);
    const pagePath = query.path;
    if (!pagePath) {
      reply.code(400);
      return { error: 'missing query parameter: path' };
    }

    const page = getPage(pagePath);
    if (!page) {
      reply.code(404);
      return { error: 'page not found' };
    }

    const baseUrl = getBaseUrl(request);
    const includeCode = query.includeCode === 'true';
    const includeHtml = query.includeHtml === 'true';
    const payload = createPagePayload(page);
    const article: Record<string, unknown> = {
      path: payload.path,
      url: payload.url,
      href: buildHref(baseUrl, payload.url),
      title: payload.title,
      description: getPageDescription(payload),
      excerpt: payload.excerpt,
      visible: payload.visible,
      source: payload.source,
      navTrail: payload.navTrail || [],
      tags: asStringArray(payload.frontMatter?.tags),
      categories: asStringArray(payload.frontMatter?.categories),
      frontMatter: payload.frontMatter,
      headings: payload.headings,
      markdown: payload.markdown,
      text: payload.text,
      chunks: payload.chunks,
      citation: createCitation(payload, baseUrl)
    };

    if (includeHtml) article.html = payload.html;

    return {
      generatedAt: getIndexPayload().generatedAt,
      article,
      codeTemplates: getCodeTemplates(payload, baseUrl, includeCode),
      includedCode: getIncludedCode(payload.path, page.markdown || payload.markdown || '', baseUrl, includeCode)
    };
  });

  app.get('/api/ai/code', async (request, reply) => {
    const query = getQuery(request);
    const codePath = query.path || query.code;
    if (!codePath) {
      reply.code(400);
      return { error: 'missing query parameter: path' };
    }

    const code = readCodePayload(codePath, getBaseUrl(request), true);
    if (!code) {
      reply.code(400);
      return { error: 'invalid code path; expected a /code/... path inside book/code' };
    }

    if (code.error) {
      reply.code(404);
      return { error: code.error };
    }

    return code;
  });

  app.post('/api/admin/reindex', async (request, reply) => {
    if (!requireAdminToken(request, reply)) return reply;
    const index = rebuildIndex();
    return {
      ok: true,
      generatedAt: index.generatedAt,
      stats: index.stats,
      errors: index.errors
    };
  });

  await app.register(fastifyStatic, {
    root: path.resolve(options.staticDir || distDir),
    prefix: '/',
    index: ['index.html'],
    decorateReply: false
  });

  app.setNotFoundHandler((request, reply) => {
    if (request.url.startsWith('/api/')) {
      reply.code(404).send({ error: 'api route not found' });
      return;
    }
    reply.sendFile('index.html');
  });

  return app;
}
