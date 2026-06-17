import type { FastifyInstance } from 'fastify';
import { rebuildIndex } from '@rbook/search';
import { requireAdminToken } from '../http/auth.js';

export async function registerAdminApiRoutes(app: FastifyInstance) {
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
}
