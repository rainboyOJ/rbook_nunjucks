import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';
import { loadCodeConfig, validateCodes } from '../packages/rbook-core/dist/validation.js';
import { buildSearchIndexFromDocuments } from '../packages/rbook-search/dist/buildIndex.js';
import { setIndexPayload } from '../packages/rbook-search/dist/searchIndex.js';
import { createApp } from '../packages/rbook-server/dist/app.js';

const html = fs.readFileSync(new URL('../site/widgets/code_template_filter/index.html', import.meta.url), 'utf8');
const source = html.match(/<script id="recall-model">([\s\S]*?)<\/script>/)[1];
const Recall = vm.runInNewContext(`${source}\nRecall;`);
const now = new Date(2026, 9, 13, 10, 0, 0);
const day = (offset) => new Date(2026, 9, 13 + offset, 10, 0, 0);
const make = (...ids) => ({ ...Recall.empty(now), cards: ids.map(Recall.newCard) });

function answer(data, id, rating, time = now) {
  Recall.start(data, id, time);
  data.active.revealed = true;
  return Recall.rate(data, rating, time);
}

test('review ladder is 1, 3, 7, 14, 30 days and remains capped', () => {
  const data = make('sample');
  let time = now;
  for (const gap of [1, 3, 7, 14, 30, 30]) {
    const card = answer(data, 'sample', 'good', time);
    assert.equal(card.due, Recall.after(gap, time));
    time = new Date(`${card.due}T10:00:00`);
  }
  const stage = data.cards[0].stage;
  const hard = answer(data, 'sample', 'hard', time);
  assert.equal(hard.due, Recall.after(1, time));
  assert.equal(hard.stage, stage);
});

test('failed cards reset the ladder, delay retry, and never consume another new slot', () => {
  const data = make('old', 'new');
  data.cards[0].stage = 4;
  data.cards[0].due = Recall.day(now);
  const card = answer(data, 'old', 'again');
  assert.equal(card.stage, 0);
  assert.equal(card.due, Recall.day(now));
  assert.equal(card.retryAt, now.getTime() + 60_000);
  assert.equal(data.daily.count, 0);
  const available = new Set(['old', 'new']);
  assert.equal(Recall.queue(data, available, now).candidates.length, 0, 'pending review blocks new cards');
  const later = new Date(now.getTime() + 60_001);
  assert.equal(Recall.queue(data, available, later).candidates[0].id, 'old');
  answer(data, 'old', 'good', later);
  assert.equal(data.cards[0].due, Recall.after(1, later));
  assert.equal(data.daily.count, 0);
  assert.equal(Recall.queue(data, available, later).candidates[0].id, 'new');
});

test('due reviews precede new cards, quotas survive retries and reset by local calendar date', () => {
  const data = make('old', 'first', 'second');
  data.newLimit = 1;
  data.cards[0].due = Recall.day(day(-3));
  const available = new Set(data.cards.map((card) => card.id));
  assert.equal(Recall.queue(data, available, now).candidates[0].id, 'old');
  answer(data, 'old', 'good');
  answer(data, 'first', 'again');
  assert.equal(data.daily.count, 1);
  const later = new Date(now.getTime() + 60_001);
  answer(data, 'first', 'good', later);
  assert.equal(data.daily.count, 1);
  assert.equal(Recall.queue(data, available, later).newCount, 0);
  assert.throws(() => Recall.start(data, 'second', later), /上限/);
  assert.equal(Recall.queue(data, available, day(1)).newCount, 1);
  assert.equal(Recall.after(1, new Date(2026, 11, 31, 23, 59)), '2027-01-01');
  assert.equal(Recall.after(1, new Date(2028, 1, 28, 23, 59)), '2028-02-29');
});

test('zero new limit still permits reviews and missing template IDs do not block the queue', () => {
  const data = make('gone', 'old', 'new');
  data.newLimit = 0;
  data.cards[0].due = data.cards[1].due = Recall.day(now);
  const queue = Recall.queue(data, new Set(['old', 'new']), now);
  assert.equal(queue.newCount, 0);
  assert.deepEqual(Array.from(queue.candidates, (card) => card.id), ['old']);
});

test('cannot rate before revealing; round-trip preserves active draft and scheduler', () => {
  const data = make('sample');
  Recall.start(data, 'sample', now);
  data.cards[0].draft = 'int main() {\n  return 0;\n}';
  assert.throws(() => Recall.rate(data, 'good', now), /查看答案/);
  const restored = Recall.validate(JSON.parse(JSON.stringify(data)));
  assert.equal(JSON.stringify(restored), JSON.stringify(data));
  restored.active.revealed = true;
  Recall.rate(restored, 'good', now);
  assert.equal(restored.active, null);
  assert.equal(restored.cards[0].draft, '');
});

test('invalid backups reject duplicates, impossible dates, invalid stages, orphan drafts and future schemas', () => {
  const mutations = [
    (d) => { d.version = 2; },
    (d) => { d.newLimit = -1; },
    (d) => { d.cards.push(d.cards[0]); },
    (d) => { d.cards[0].stage = 5; },
    (d) => { d.cards[0].due = '2026-02-30'; },
    (d) => { d.cards[0].retryAt = -1; },
    (d) => { d.cards[0].retryAt = 123; },
    (d) => { d.active = { id: 'gone', revealed: false }; },
    (d) => { d.cards[0].draft = {}; },
    (d) => { d.cards[0].id = '__proto__'; }
  ];
  for (const mutate of mutations) {
    const data = make('sample');
    mutate(data);
    assert.throws(() => Recall.validate(data), /无效/);
  }
});

test('recall_prompt is validated, indexed and returned by list and detail APIs', async (t) => {
  const sample = loadCodeConfig({ strict: true }).codes[0];
  const prompt = '实现指定功能。\n注意边界条件。<script>plain text</script>';
  const codes = [{ ...sample, recall_prompt: prompt }, { ...sample, id: 'without-prompt' }];
  assert.equal(validateCodes(codes).some((issue) => issue.message.includes('recall_prompt')), false);
  assert.ok(validateCodes([{ ...sample, recall_prompt: {} }]).some((issue) => issue.level === 'ERROR' && issue.message.includes('recall_prompt')));
  const index = buildSearchIndexFromDocuments({}, [], codes, { write: false });
  assert.equal(index.codes[0].recall_prompt, prompt);
  assert.equal(index.codes[1].recall_prompt, '');
  setIndexPayload(index);
  const app = await createApp({ logger: false });
  t.after(() => app.close());
  const list = (await app.inject('/api/codes')).json();
  const detail = await app.inject(`/api/codes?id=${sample.id}&includeContent=true`);
  assert.equal(list.items[0].recall_prompt, prompt);
  assert.equal(detail.statusCode, 200);
  assert.equal(detail.json().recall_prompt, prompt);
  assert.equal(typeof detail.json().content, 'string');
});
