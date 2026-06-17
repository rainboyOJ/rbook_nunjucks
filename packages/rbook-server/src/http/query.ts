import type { FastifyRequest } from 'fastify';

export type QueryParams = Record<string, string | undefined>;

export function getQuery(request: FastifyRequest): QueryParams {
  return request.query as QueryParams;
}

export function parseLimit(value: string | number | undefined, fallback = 10) {
  const limit = Number(value || fallback);
  if (!Number.isFinite(limit) || limit <= 0) return fallback;
  return Math.min(Math.floor(limit), 50);
}

export function getBaseUrl(request: FastifyRequest) {
  const protocolHeader = request.headers['x-forwarded-proto'];
  const protocol = Array.isArray(protocolHeader)
    ? protocolHeader[0]
    : protocolHeader || request.protocol;
  return `${protocol}://${request.headers.host || '127.0.0.1:3000'}`;
}

export function buildHref(baseUrl: string, url: string | null | undefined) {
  if (!url) return null;
  return new URL(url, baseUrl.endsWith('/') ? baseUrl : `${baseUrl}/`).toString();
}
