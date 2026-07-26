import fs from 'fs';
import path from 'path';
import type { FastifyInstance } from 'fastify';
import { getIndexPayload, getPageById } from '@rbook/search';
import { codeTemplateDir } from '@rbook/core/paths';
import { getQuery, parseLimit } from '../http/query.js';
import { createPagePayload } from '../services/pageService.js';
import { readApiDocsMarkdown, renderApiDocsPage } from '../docs/apiDocs.js';
import { getBaseUrl } from '../http/query.js';

function asStringArray(value: unknown) {
  return Array.isArray(value)
    ? value.filter((item) => typeof item === 'string')
    : [];
}

function parseTags(value: string | undefined) {
  if (!value) return [];
  return value.split(',').map((item) => item.trim()).filter(Boolean);
}

function parseOffset(value: string | undefined) {
  const offset = Number(value || 0);
  if (!Number.isFinite(offset) || offset < 0) return 0;
  return Math.floor(offset);
}

function pageMatchesTags(page: any, tags: string[]) {
  if (tags.length === 0) return true;
  const pageTags = asStringArray(page.frontMatter?.tags || page.tags);
  return tags.some((tag) => pageTags.includes(tag));
}

function codeMatchesTags(code: any, tags: string[]) {
  if (tags.length === 0) return true;
  const codeTags = asStringArray(code.tags);
  return tags.some((tag) => codeTags.includes(tag));
}

function compactPage(page: any) {
  return {
    id: page.id,
    title: page.title,
    description: String(page.frontMatter?.description || page.frontMatter?.desc || page.excerpt || ''),
    tags: asStringArray(page.frontMatter?.tags),
    path: page.path,
    url: page.url
  };
}

function fullPage(page: any) {
  const payload = createPagePayload(page);
  return {
    id: page.id || payload.frontMatter?.id || '',
    title: payload.title,
    path: payload.path,
    url: payload.url,
    description: String(payload.frontMatter?.description || payload.frontMatter?.desc || payload.excerpt || ''),
    tags: asStringArray(payload.frontMatter?.tags),
    categories: asStringArray(payload.frontMatter?.categories),
    frontMatter: payload.frontMatter,
    headings: payload.headings,
    markdown: payload.markdown,
    navTrail: payload.navTrail || []
  };
}

function readCodeContent(codePath: string) {
  const absolutePath = path.resolve(codeTemplateDir, codePath);
  const root = path.resolve(codeTemplateDir);
  const relative = path.relative(root, absolutePath);
  if (relative.startsWith('..') || path.isAbsolute(relative)) return null;
  if (!fs.existsSync(absolutePath)) return null;
  return fs.readFileSync(absolutePath, 'utf8');
}

function withArticles(code: any, codeToArticles: Record<string, any[]>) {
  return {
    ...code,
    articles: codeToArticles[code.id] || []
  };
}

function countTags(items: Array<{ tags?: string[] }>) {
  const counter = new Map<string, number>();
  for (const item of items) {
    for (const tag of asStringArray(item.tags)) {
      counter.set(tag, (counter.get(tag) || 0) + 1);
    }
  }
  return [...counter.entries()]
    .map(([tag, count]) => ({ tag, count }))
    .sort((a, b) => b.count - a.count || a.tag.localeCompare(b.tag, 'zh-CN'));
}

export async function registerPublicApiRoutes(app: FastifyInstance) {
  app.get('/api/health', async () => {
    const index = getIndexPayload();
    return {
      ok: true,
      generatedAt: index.generatedAt,
      stats: index.stats
    };
  });

  app.get('/api/help', async (request, reply) => {
    const query = getQuery(request);
    const format = query.format === 'md' ? 'md' : 'html';
    if (format === 'md') {
      reply.type('text/markdown; charset=utf-8');
      return readApiDocsMarkdown();
    }

    reply.type('text/html; charset=utf-8');
    return renderApiDocsPage(getBaseUrl(request));
  });

  app.get('/api/site', async () => {
    const index = getIndexPayload();
    return {
      site: index.site,
      stats: index.stats,
      generatedAt: index.generatedAt
    };
  });

  app.get('/api/catalog', async (request) => {
    const index = getIndexPayload();
    const query = getQuery(request);
    const compact = query.compact === 'true';
    const pages = (index.pages || []).filter((page: any) => page.visible !== false);
    const items = compact
      ? pages.map(compactPage)
      : pages.map((page: any) => ({
          ...compactPage(page),
          headings: page.headings || [],
          navTrail: page.navTrail || [],
          codeTemplates: asStringArray(page.frontMatter?.code_template),
          visible: page.visible,
          source: page.source
        }));

    return {
      generatedAt: index.generatedAt,
      total: items.length,
      items
    };
  });

  app.get('/api/pages', async (request, reply) => {
    const index = getIndexPayload();
    const query = getQuery(request);
    const id = query.id;
    const tags = parseTags(query.tag);
    const limit = parseLimit(query.limit, 50);
    const offset = parseOffset(query.offset);

    if (id) {
      const page = getPageById(id);
      if (!page) {
        reply.code(404);
        return { error: 'PAGE_NOT_FOUND', message: `page with id '${id}' not found` };
      }
      return fullPage(page);
    }

    let pages = index.pages || [];
    if (tags.length > 0) {
      pages = pages.filter((page: any) => pageMatchesTags(page, tags));
    }

    const total = pages.length;
    const items = pages.slice(offset, offset + limit).map((page: any) => ({
      id: page.id,
      title: page.title,
      path: page.path,
      url: page.url,
      description: String(page.frontMatter?.description || page.frontMatter?.desc || page.excerpt || ''),
      tags: asStringArray(page.frontMatter?.tags),
      visible: page.visible,
      source: page.source,
      navTrail: page.navTrail || [],
      codeTemplates: asStringArray(page.frontMatter?.code_template)
    }));

    return {
      generatedAt: index.generatedAt,
      total,
      items
    };
  });

  app.get('/api/codes', async (request, reply) => {
    const index = getIndexPayload();
    const query = getQuery(request);
    const id = query.id;
    const tags = parseTags(query.tag);
    const includeContent = query.includeContent === 'true';
    const limit = parseLimit(query.limit, 50);
    const offset = parseOffset(query.offset);
    const codeToArticles = index.codeToArticles || {};

    if (id) {
      const code = (index.codes || []).find((item: any) => item.id === id);
      if (!code) {
        reply.code(404);
        return { error: 'CODE_NOT_FOUND', message: `code with id '${id}' not found` };
      }

      const payload = withArticles(code, codeToArticles);
      if (includeContent) {
        const content = readCodeContent(code.path);
        if (content === null) {
          reply.code(404);
          return { error: 'CODE_FILE_NOT_FOUND', message: `code file not found for id '${id}'` };
        }
        payload.content = content;
      }
      return payload;
    }

    let codes = index.codes || [];
    if (tags.length > 0) {
      codes = codes.filter((code: any) => codeMatchesTags(code, tags));
    }

    const total = codes.length;
    const items = codes.slice(offset, offset + limit).map((code: any) => {
      const payload = withArticles(code, codeToArticles);
      if (includeContent) {
        payload.content = readCodeContent(code.path) || '';
      }
      return payload;
    });

    return {
      generatedAt: index.generatedAt,
      total,
      items
    };
  });

  app.get('/api/tags', async () => {
    const index = getIndexPayload();
    const articleTags = countTags(
      (index.pages || []).map((page: any) => ({
        tags: asStringArray(page.frontMatter?.tags)
      }))
    );
    const codeTags = countTags(index.codes || []);

    return {
      generatedAt: index.generatedAt,
      articleTags,
      codeTags
    };
  });
}
