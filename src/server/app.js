import path from 'path';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import { distDir } from '../search/paths.js';
import {
  getIndexPayload,
  getPage,
  rebuildIndex,
  searchChunks,
  searchPages
} from '../search/searchIndex.js';

function parseLimit(value, fallback = 10) {
  const limit = Number(value || fallback);
  if (!Number.isFinite(limit) || limit <= 0) return fallback;
  return Math.min(Math.floor(limit), 50);
}

function requireAdminToken(request, reply) {
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

export async function createApp(options = {}) {
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

  app.get('/api/pages', async (request) => {
    const index = getIndexPayload();
    const visibleOnly = request.query.visible === 'true';
    const pages = visibleOnly ? index.pages.filter((page) => page.visible) : index.pages;
    return {
      total: pages.length,
      pages
    };
  });

  app.get('/api/page', async (request, reply) => {
    const pagePath = request.query.path;
    if (!pagePath) {
      reply.code(400);
      return { error: 'missing query parameter: path' };
    }

    const page = getPage(pagePath);
    if (!page) {
      reply.code(404);
      return { error: 'page not found' };
    }

    return page;
  });

  app.get('/api/search', async (request, reply) => {
    const query = request.query.q || request.query.query;
    if (!query) {
      reply.code(400);
      return { error: 'missing query parameter: q' };
    }

    return searchPages(query, {
      limit: parseLimit(request.query.limit)
    });
  });

  app.get('/api/chunks/search', async (request, reply) => {
    const query = request.query.q || request.query.query;
    if (!query) {
      reply.code(400);
      return { error: 'missing query parameter: q' };
    }

    return searchChunks(query, {
      limit: parseLimit(request.query.limit),
      textLength: Number(request.query.textLength || 900),
      includeText: request.query.includeText !== 'false'
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
