import path from 'path';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import { distDir } from '@rbook/core/paths';
import {
  getIndexPayload,
  getPage,
  rebuildIndex,
  searchChunks,
  searchPages
} from '@rbook/search';
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

    return page;
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
