import path from 'path';
import fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import { distDir } from '@rbook/core/paths';
import { renderApiDocsPage } from './docs/apiDocs.js';
import { getBaseUrl } from './http/query.js';
import { registerAdminApiRoutes } from './routes/adminApi.js';
import { registerAiApiRoutes } from './routes/aiApi.js';
import { registerPublicApiRoutes } from './routes/publicApi.js';

interface CreateAppOptions {
  logger?: boolean;
  staticDir?: string;
}

export async function createApp(options: CreateAppOptions = {}) {
  const app = fastify({
    logger: options.logger ?? true
  });

  app.get('/api', async (request, reply) => {
    reply.type('text/html; charset=utf-8');
    return renderApiDocsPage(getBaseUrl(request));
  });

  await registerPublicApiRoutes(app);
  await registerAiApiRoutes(app);
  await registerAdminApiRoutes(app);

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
