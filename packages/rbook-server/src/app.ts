import path from 'path';
import fs from 'fs';
import yaml from 'js-yaml';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import Markdown from '@rbook/markdown';
import { bookDir, configPath, distDir } from '@rbook/core/paths';
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

function getQuery(request: FastifyRequest): QueryParams {
  return request.query as QueryParams;
}

function parseLimit(value: string | number | undefined, fallback = 10) {
  const limit = Number(value || fallback);
  if (!Number.isFinite(limit) || limit <= 0) return fallback;
  return Math.min(Math.floor(limit), 50);
}

function stripTrailingSlash(url: unknown) {
  return String(url || '').replace(/\/+$/, '');
}

function normalizeProblemId(oj: string, id: string) {
  const raw = String(id || '').trim();
  if (String(oj || '').toLowerCase() === 'luogu' && /^\d+$/.test(raw)) {
    return `P${raw}`;
  }

  return raw.replace(/^([PBTU])(\d+)$/i, (_match, prefix, number) => {
    return `${String(prefix).toUpperCase()}${number}`;
  });
}

function loadBookConfig() {
  if (!fs.existsSync(configPath)) return {};
  return (yaml.load(fs.readFileSync(configPath, 'utf8')) || {}) as Record<string, unknown>;
}

function getPcsLink() {
  const config = loadBookConfig();
  return stripTrailingSlash(config['pcs-link']);
}

function buildProblemUrl(oj: string, id: string) {
  const baseUrl = getPcsLink();
  const encodedOj = encodeURIComponent(oj);
  const encodedId = encodeURIComponent(id);
  return baseUrl
    ? `${baseUrl}/problems/${encodedOj}/${encodedId}`
    : `/problems/${encodedOj}/${encodedId}`;
}

function resolveProblem(oj: string | undefined, id: string | undefined) {
  const normalizedOj = String(oj || '').trim();
  if (!normalizedOj || !id) return null;

  const normalizedId = normalizeProblemId(normalizedOj, id);
  return {
    oj: normalizedOj,
    id: normalizedId,
    label: `${normalizedOj}-${normalizedId}`,
    url: buildProblemUrl(normalizedOj, normalizedId),
    target: '_blank',
    rel: 'noopener'
  };
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

  app.get('/api/problems/resolve', async (request, reply) => {
    const query = getQuery(request);
    const problem = resolveProblem(query.oj, query.id || query.problem_id);

    if (!problem) {
      reply.code(400);
      return { error: 'missing query parameters: oj, id' };
    }

    return {
      ...problem,
      pcsLink: getPcsLink()
    };
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
