export interface Pcs2Problem {
  oj: string;
  problem_id: string;
  title: string;
  difficulty: string;
  tags: string[];
  url: string;
}

interface Pcs2ListResponse {
  data?: unknown;
  pagination?: {
    total?: number;
    page?: number;
    limit?: number;
    totalPages?: number;
  };
}

interface CacheEntry {
  expiresAt: number;
  items: Pcs2Problem[];
}

export interface Pcs2ServiceOptions {
  apiBaseUrl?: string;
  publicBaseUrl?: string;
  timeoutMs?: number;
  cacheTtlMs?: number;
  pageSize?: number;
  maxItems?: number;
  fetchImpl?: typeof fetch;
  logger?: Pick<Console, 'warn'>;
}

const DEFAULT_API_BASE_URL = 'https://pcs2.roj.ac.cn';
const DEFAULT_PUBLIC_BASE_URL = 'https://pcs2.roj.ac.cn';
const DEFAULT_TIMEOUT_MS = 2_000;
const DEFAULT_CACHE_TTL_MS = 60_000;
const DEFAULT_PAGE_SIZE = 100;
const DEFAULT_MAX_ITEMS = 500;

function normalizeBaseUrl(value: string) {
  return value.replace(/\/+$/, '');
}

function asProblem(value: unknown, publicBaseUrl: string, articleId: string): Pcs2Problem | null {
  if (!value || typeof value !== 'object') return null;
  const item = value as Record<string, unknown>;
  if (!Array.isArray(item.showAtRbook) || !item.showAtRbook.includes(articleId)) return null;
  const oj = String(item.oj || '').trim();
  const problemId = String(item.problem_id || '').trim();
  if (!oj || !problemId) return null;

  const rawUrl = String(item.url || `/problems/${encodeURIComponent(oj)}/${encodeURIComponent(problemId)}`);
  let url: string;
  try {
    const parsed = new URL(rawUrl, `${publicBaseUrl}/`);
    url = new URL(`${parsed.pathname}${parsed.search}${parsed.hash}`, `${publicBaseUrl}/`).toString();
  } catch {
    url = `${publicBaseUrl}/problems/${encodeURIComponent(oj)}/${encodeURIComponent(problemId)}`;
  }

  return {
    oj,
    problem_id: problemId,
    title: String(item.title || '').trim(),
    difficulty: String(item.difficulty || '未知').trim() || '未知',
    tags: Array.isArray(item.tags) ? item.tags.filter((tag): tag is string => typeof tag === 'string') : [],
    url
  };
}

export class Pcs2Service {
  private readonly apiBaseUrl: string;
  private readonly publicBaseUrl: string;
  private readonly timeoutMs: number;
  private readonly cacheTtlMs: number;
  private readonly pageSize: number;
  private readonly maxItems: number;
  private readonly fetchImpl: typeof fetch;
  private readonly logger: Pick<Console, 'warn'>;
  private readonly cache = new Map<string, CacheEntry>();
  private readonly inflight = new Map<string, Promise<Pcs2Problem[]>>();

  constructor(options: Pcs2ServiceOptions = {}) {
    this.apiBaseUrl = normalizeBaseUrl(options.apiBaseUrl || process.env.PCS2_API_BASE_URL || DEFAULT_API_BASE_URL);
    this.publicBaseUrl = normalizeBaseUrl(options.publicBaseUrl || process.env.PCS2_PUBLIC_BASE_URL || DEFAULT_PUBLIC_BASE_URL);
    this.timeoutMs = options.timeoutMs ?? Number(process.env.PCS2_TIMEOUT_MS || DEFAULT_TIMEOUT_MS);
    this.cacheTtlMs = options.cacheTtlMs ?? Number(process.env.PCS2_CACHE_TTL_MS || DEFAULT_CACHE_TTL_MS);
    this.pageSize = options.pageSize ?? DEFAULT_PAGE_SIZE;
    this.maxItems = options.maxItems ?? DEFAULT_MAX_ITEMS;
    this.fetchImpl = options.fetchImpl || fetch;
    this.logger = options.logger || console;
  }

  async getProblems(articleId: string): Promise<Pcs2Problem[]> {
    const key = String(articleId || '').trim();
    if (!key) return [];

    const cached = this.cache.get(key);
    if (cached && cached.expiresAt > Date.now()) return cached.items;
    if (cached) this.cache.delete(key);

    const pending = this.inflight.get(key);
    if (pending) return pending;

    const request = this.fetchAll(key)
      .then((items) => {
        this.cache.set(key, { expiresAt: Date.now() + this.cacheTtlMs, items });
        return items;
      })
      .catch((error) => {
        this.logger.warn(`[pcs2] failed to load ${key}: ${error instanceof Error ? error.message : String(error)}`);
        return [];
      })
      .finally(() => {
        this.inflight.delete(key);
      });

    this.inflight.set(key, request);
    return request;
  }

  private async fetchAll(articleId: string) {
    const items: Pcs2Problem[] = [];
    let page = 1;
    let totalPages = 1;

    while (page <= totalPages && items.length < this.maxItems) {
      const params = new URLSearchParams({
        showAtRbook: articleId,
        sort: 'difficulty',
        order: 'asc',
        page: String(page),
        limit: String(this.pageSize)
      });
      const response = await this.fetchPage(params);
      const pageItems = Array.isArray(response.data)
        ? response.data.map((item) => asProblem(item, this.publicBaseUrl, articleId)).filter(Boolean) as Pcs2Problem[]
        : [];
      items.push(...pageItems.slice(0, this.maxItems - items.length));

      const reportedTotalPages = Number(response.pagination?.totalPages);
      totalPages = Number.isFinite(reportedTotalPages) && reportedTotalPages > 0
        ? reportedTotalPages
        : pageItems.length < this.pageSize ? page : page + 1;
      page += 1;
      if (pageItems.length === 0) break;
    }

    if (items.length >= this.maxItems && page <= totalPages) {
      this.logger.warn(`[pcs2] result for ${articleId} exceeded ${this.maxItems} items; truncated`);
    }

    return items;
  }

  private async fetchPage(params: URLSearchParams): Promise<Pcs2ListResponse> {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), this.timeoutMs);
    try {
      const response = await this.fetchImpl(`${this.apiBaseUrl}/api/problems?${params.toString()}`, {
        headers: { Accept: 'application/json' },
        signal: controller.signal
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      const payload = await response.json() as Pcs2ListResponse;
      if (!payload || typeof payload !== 'object') throw new Error('invalid JSON response');
      return payload;
    } finally {
      clearTimeout(timeout);
    }
  }
}

export default Pcs2Service;
