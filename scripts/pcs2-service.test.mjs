import assert from 'node:assert/strict';
import test from 'node:test';
import { Pcs2Service } from '../packages/rbook-server/dist/services/pcs2Service.js';

function response(payload, status = 200) {
  return {
    ok: status >= 200 && status < 300,
    status,
    async json() {
      return payload;
    },
  };
}

test('PCS2 service follows pagination, sends exposure and sort filters, and caches success', async () => {
  const requests = [];
  const fetchImpl = async (url) => {
    requests.push(String(url));
    const page = new URL(url).searchParams.get('page');
    return response({
      data: page === '1'
        ? [{ oj: 'luogu', problem_id: 'P1', title: 'One', difficulty: '入门', tags: [], showAtRbook: ['bit'], url: 'http://problems-solution:3000/problems/luogu/P1' }]
        : [{ oj: 'luogu', problem_id: 'P2', title: 'Two', difficulty: '提高', tags: [], showAtRbook: ['bit'], url: '/problems/luogu/P2' }],
      pagination: { page: Number(page), total: 2, limit: 1, totalPages: 2 },
    });
  };
  const service = new Pcs2Service({
    apiBaseUrl: 'http://pcs2.internal',
    publicBaseUrl: 'https://pcs2.example.test',
    pageSize: 1,
    fetchImpl,
  });

  const first = await service.getProblems('bit');
  const second = await service.getProblems('bit');

  assert.deepEqual(first.map((item) => item.problem_id), ['P1', 'P2']);
  assert.equal(second, first);
  assert.equal(requests.length, 2);
  assert.match(requests[0], /showAtRbook=bit/);
  assert.match(requests[0], /sort=difficulty/);
  assert.match(requests[0], /order=asc/);
  assert.equal(first[0].url, 'https://pcs2.example.test/problems/luogu/P1');
});

test('PCS2 service returns empty results after timeout or upstream failure', async () => {
  const warnings = [];
  const service = new Pcs2Service({
    timeoutMs: 5,
    fetchImpl: async (_url, { signal }) => new Promise((_, reject) => {
      signal.addEventListener('abort', () => reject(new Error('aborted')));
    }),
    logger: { warn: (message) => warnings.push(message) },
  });

  assert.deepEqual(await service.getProblems('bit'), []);
  assert.equal(warnings.length, 1);
});

test('PCS2 service fails closed when an upstream ignores showAtRbook', async () => {
  const service = new Pcs2Service({
    fetchImpl: async () => response({
      data: [{ oj: 'luogu', problem_id: 'P1', title: 'Unfiltered', difficulty: '入门', tags: [] }],
      pagination: { total: 1, page: 1, limit: 100, totalPages: 1 },
    }),
  });

  assert.deepEqual(await service.getProblems('bit'), []);
});
