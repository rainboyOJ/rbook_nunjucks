import assert from 'node:assert/strict';
import path from 'node:path';
import test from 'node:test';
import pug from 'pug';
import { load } from 'cheerio';
import { getArticleRelations } from '../packages/rbook-core/dist/articleRelations.js';

const template = path.resolve('site/theme/partials/article-relations.pug');
const page = (id, supplements = [], title = id) => ({
  title,
  url: `/${id}.html`,
  frontMatter: { id, title, supplements }
});

test('reading guide resolves shared supplements in both directions without expanding neighbors', () => {
  const current = page('current', ['proof', 'proof', 'current', 'missing']);
  const pages = [
    current,
    page('proof', ['deeper']),
    page('deeper'),
    page('parent-one', ['current', 'sibling']),
    page('parent-two', ['current']),
    page('sibling'),
    page('prerequisite')
  ];
  const graph = getArticleRelations(pages, { ...current.frontMatter, prerequisites: ['prerequisite'] });
  assert.deepEqual(graph.incoming.map(p => p.id), ['parent-one', 'parent-two']);
  assert.deepEqual(graph.outgoing.map(p => p.id), ['proof']);
  assert.equal(graph.current.id, 'current');

  const proofGraph = getArticleRelations(pages, pages[1].frontMatter);
  assert.deepEqual(proofGraph.incoming.map(p => p.id), ['current']);
  assert.deepEqual(proofGraph.outgoing.map(p => p.id), ['deeper']);
});

test('reading guide omits unconnected articles and tolerates unusable declarations', () => {
  assert.equal(getArticleRelations([page('alone')], page('alone').frontMatter), null);
  assert.equal(getArticleRelations([page('alone')], { id: 'alone', supplements: [null, 1, '', 'alone', 'missing'] }), null);
  assert.equal(getArticleRelations([page('alone')], { supplements: ['alone'] }), null);
  assert.equal(pug.renderFile(template, { articleRelations: null }).trim(), '');
});

test('each direction exposes three initial links and an independent native disclosure for the remainder', () => {
  const current = page('current', ['one', 'two', 'three', 'four']);
  const pages = [current, ...['one', 'two', 'three', 'four'].map(id => page(id)),
    ...['parent-one', 'parent-two', 'parent-three', 'parent-four'].map(id => page(id, ['current']))];
  const html = pug.renderFile(template, { articleRelations: getArticleRelations(pages, current.frontMatter) });
  const $ = load(html);
  assert.equal($('[aria-current="page"]').attr('data-relation-id'), 'current');
  for (const direction of ['incoming', 'outgoing']) {
    const group = $(`.article-relation-group--${direction}`);
    assert.equal(group.children('ul').find('a').length, 3);
    assert.equal(group.find('details[open]').length, 0);
    assert.equal(group.find('summary .article-relation-show-all').text(), '查看全部 4 篇');
    assert.equal(group.find('details ul a').length, 1);
  }
  assert.equal($('.article-relation-current a').length, 0);
});

test('three relations need no disclosure and article titles remain escaped links', () => {
  const current = page('current', ['one', 'two', 'three']);
  const pages = [current, page('one', [], '<script>alert(1)</script>'), page('two'), page('three')];
  const $ = load(pug.renderFile(template, { articleRelations: getArticleRelations(pages, current.frontMatter) }));
  assert.equal($('details').length, 0);
  assert.equal($('.article-relation-group--incoming').length, 0);
  assert.equal($('script').length, 0);
  assert.equal($('a[data-relation-id="one"]').text(), '<script>alert(1)</script>');
  assert.equal($('a[data-relation-id="one"]').attr('href'), '/one.html');
});
