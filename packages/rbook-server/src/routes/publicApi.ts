import type { FastifyInstance } from 'fastify';
import {
  getIndexPayload,
  getPage,
  searchChunks,
  searchPages
} from '@rbook/search';
import { getQuery, parseLimit } from '../http/query.js';
import { buildToc, createPagePayload } from '../services/pageService.js';

export async function registerPublicApiRoutes(app: FastifyInstance) {
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
}
