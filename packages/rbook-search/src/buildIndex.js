import fs from 'fs';
import path from 'path';
import Fuse from 'fuse.js';
import { collectPages } from './collectPages.js';
import { loadPageDocument } from './markdownText.js';
import { searchDir, searchIndexPath } from './paths.js';

function createFuseData(documents) {
  return documents.flatMap((doc) => doc.chunks.map((chunk) => ({
    ...chunk,
    navTrail: doc.navTrail || [],
    visible: doc.visible,
    source: doc.source
  })));
}

export function buildSearchIndex(options = {}) {
  const collected = collectPages(options);
  const documents = [];
  const errors = [];

  for (const page of collected.pages) {
    try {
      documents.push(loadPageDocument(page));
    } catch (error) {
      errors.push({ path: page.path, message: error.message });
    }
  }

  const chunks = createFuseData(documents);
  const fuse = new Fuse(chunks, {
    includeScore: true,
    ignoreLocation: true,
    threshold: 0.42,
    keys: [
      { name: 'title', weight: 0.32 },
      { name: 'heading', weight: 0.28 },
      { name: 'text', weight: 0.36 },
      { name: 'path', weight: 0.04 }
    ]
  });

  const payload = {
    version: 1,
    generatedAt: new Date().toISOString(),
    site: {
      title: collected.site.title,
      author: collected.site.author,
      description: collected.site.description,
      github_repository: collected.site.github_repository
    },
    stats: {
      pages: documents.length,
      chunks: chunks.length,
      errors: errors.length
    },
    pages: documents.map((doc) => ({
      path: doc.path,
      url: doc.url,
      title: doc.title,
      visible: doc.visible,
      source: doc.source,
      navTrail: doc.navTrail || [],
      headings: doc.headings,
      excerpt: doc.excerpt,
      frontMatter: doc.frontMatter
    })),
    chunks,
    fuseIndex: fuse.getIndex().toJSON(),
    errors
  };

  if (options.write !== false) {
    fs.mkdirSync(searchDir, { recursive: true });
    fs.writeFileSync(options.outputPath || searchIndexPath, JSON.stringify(payload, null, 2));
  }

  return payload;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const outputPath = process.argv[2] ? path.resolve(process.argv[2]) : searchIndexPath;
  const payload = buildSearchIndex({ outputPath });
  console.log(`Search index written: ${outputPath}`);
  console.log(`Pages: ${payload.stats.pages}, chunks: ${payload.stats.chunks}, errors: ${payload.stats.errors}`);
}
