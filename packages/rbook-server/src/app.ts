import path from 'path';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import { distDir } from '@rbook/core/paths';
import { renderApiDocsPage } from './docs/apiDocs.js';
import { getBaseUrl } from './http/query.js';
import { registerAdminApiRoutes } from './routes/adminApi.js';
import { registerPublicApiRoutes } from './routes/publicApi.js';
import type { DevResponse, DevRenderer } from './devRenderer.js';

interface CreateAppOptions {
  logger?: boolean;
  staticDir?: string;
  devRenderer?: Pick<DevRenderer, 'render' | 'notFound' | 'error'>;
}

export async function createApp(options: CreateAppOptions = {}) {
  const app = fastify({
    logger: options.logger ?? true
  });

  app.addHook('onRequest', async (request, reply) => {
    const url = request.url;
    if (url === '/api' || url.startsWith('/api?') || url.startsWith('/api/')) {
      reply.header('Cache-Control', 'no-store');
    }
  });

  app.get('/api', async (request, reply) => {
    reply.type('text/html; charset=utf-8');
    return renderApiDocsPage(getBaseUrl(request));
  });

  await registerPublicApiRoutes(app);
  await registerAdminApiRoutes(app);

  if (options.devRenderer) {
    app.addHook('onRequest', async (request, reply) => {
      if (request.url === '/api' || request.url.startsWith('/api/')) return;
      if (request.method !== 'GET' && request.method !== 'HEAD') return;

      try {
        const result: DevResponse | null = options.devRenderer!.render(request.url);
        if (!result) return;
        reply
          .code(result.statusCode)
          .type(result.contentType)
          .header('Cache-Control', 'no-store')
          .send(result.body);
      } catch (error) {
        request.log.error(error);
        const result = options.devRenderer!.error(request.url, error);
        reply
          .code(result.statusCode)
          .type(result.contentType)
          .header('Cache-Control', 'no-store')
          .send(result.body);
      }
    });
  }

  await app.register(fastifyStatic, {
    root: path.resolve(options.staticDir || distDir),
    prefix: '/',
    index: ['index.html'],
    decorateReply: false
  });

  app.setNotFoundHandler((request, reply) => {
    if (request.url.startsWith('/api/')) {
      reply.code(404).send({ error: 'API_ROUTE_NOT_FOUND', message: 'api route not found' });
      return;
    }
    if (options.devRenderer) {
      const result = options.devRenderer.notFound(request.url);
      reply.code(result.statusCode).type(result.contentType).send(result.body);
      return;
    }
    reply.sendFile('index.html');
  });

  return app;
}
