import type { FastifyReply, FastifyRequest } from 'fastify';

export function requireAdminToken(request: FastifyRequest, reply: FastifyReply) {
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
