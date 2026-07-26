import assert from 'node:assert/strict';
import test from 'node:test';
import matter from 'gray-matter';
import { repairMalformedFrontMatter } from './fix-malformed-frontmatter.mjs';

test('repairs a matching ID and preserves the body', () => {
  const source = `---
id: about
title: about
---

---id: about

title: About this book
---
# Body
`;

  const result = repairMalformedFrontMatter(source, { filePath: 'about.md' });
  const parsed = matter(result.content);

  assert.equal(result.changed, true);
  assert.equal(result.resolvedConflict, false);
  assert.deepEqual(parsed.data, { id: 'about', title: 'About this book' });
  assert.equal(parsed.content, '# Body\n');
  assert.equal(result.content.includes('---id:'), false);
});

test('preserves all fields from a field-rich original block', () => {
  const source = `---
id: graph-example
title: generated
---

---id: graph-example

title: Original title
date: 2026-07-26 10:30
toc: true
tags: [graph, flow]
categories: [graph]
code_template: [dinic]
custom_field:
  enabled: true
---

## Section
Text.
`;

  const result = repairMalformedFrontMatter(source, { filePath: 'graph/example.md' });
  const parsed = matter(result.content);

  assert.equal(parsed.data.id, 'graph-example');
  assert.equal(parsed.data.title, 'Original title');
  assert.deepEqual(parsed.data.tags, ['graph', 'flow']);
  assert.deepEqual(parsed.data.categories, ['graph']);
  assert.deepEqual(parsed.data.code_template, ['dinic']);
  assert.deepEqual(parsed.data.custom_field, { enabled: true });
  assert.equal(parsed.content, '\n## Section\nText.\n');
});

test('requires an explicit resolution for conflicting generated IDs', () => {
  const source = `---
id: graph-7
title: generated
---

---id: graph-2

title: Original title
---
Body
`;

  assert.throws(
    () => repairMalformedFrontMatter(source, { filePath: 'graph/example.md' }),
    /an explicit expectedId is required/
  );

  const result = repairMalformedFrontMatter(source, {
    filePath: 'graph/example.md',
    expectedId: 'graph-7'
  });
  assert.equal(result.resolvedConflict, true);
  assert.equal(matter(result.content).data.id, 'graph-7');
  assert.equal(matter(result.content).content, 'Body\n');
});

test('leaves a non-target file unchanged', () => {
  const source = `---
id: valid-page
title: Valid page
---
Body
`;

  const result = repairMalformedFrontMatter(source, { filePath: 'valid.md' });
  assert.equal(result.changed, false);
  assert.equal(result.content, source);
});
