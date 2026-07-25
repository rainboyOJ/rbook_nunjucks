import path from 'path';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import { distDir } from '@rbook/core/paths';
import { readApiDocsMarkdown, renderApiDocsPage } from './docs/apiDocs.js';
import { getBaseUrl } from './http/query.js';
import { registerAdminApiRoutes } from './routes/adminApi.js';
import { registerAiApiRoutes } from './routes/aiApi.js';
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

  app.get('/api', async (request, reply) => {
    reply
      .type('text/html; charset=utf-8')
      .header('Cache-Control', 'no-store');
    return renderApiDocsPage(getBaseUrl(request));
  });

  app.get('/api/md', async (_request, reply) => {
    reply
      .type('text/markdown; charset=utf-8')
      .header('Cache-Control', 'no-store');
    return readApiDocsMarkdown();
  });

  await registerPublicApiRoutes(app);
  await registerAiApiRoutes(app);
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
      reply.code(404).send({ error: 'api route not found' });
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
