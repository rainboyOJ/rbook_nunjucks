import type { FastifyInstance } from 'fastify';
import { getIndexPayload, getPage } from '@rbook/search';
import { getBaseUrl, getQuery } from '../http/query.js';
import { readCodePayload } from '../services/codeService.js';
import {
  createAiCatalogItem,
  createAiPageContext
} from '../services/aiContextService.js';

export async function registerAiApiRoutes(app: FastifyInstance) {
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

    return {
      generatedAt: getIndexPayload().generatedAt,
      ...createAiPageContext(page, getBaseUrl(request), {
        includeCode: query.includeCode === 'true',
        includeHtml: query.includeHtml === 'true'
      })
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
}
