import fs from 'fs';
import Fuse from 'fuse.js';
import { buildSearchIndex } from './buildIndex.js';
import { bookDir, searchIndexPath } from './paths.js';

let cachedPayload = null;
let cachedFuse = null;

function loadPayload({ rebuild = false } = {}) {
  if (!rebuild && cachedPayload && cachedFuse) {
    return { payload: cachedPayload, fuse: cachedFuse };
  }

  let payload;
  if (!rebuild && fs.existsSync(searchIndexPath)) {
    payload = JSON.parse(fs.readFileSync(searchIndexPath, 'utf8'));
  } else {
    payload = buildSearchIndex();
  }

  const fuse = new Fuse(payload.chunks, {
    includeScore: true,
    ignoreLocation: true,
    threshold: 0.42,
    keys: [
      { name: 'title', weight: 0.32 },
      { name: 'heading', weight: 0.28 },
      { name: 'text', weight: 0.36 },
      { name: 'path', weight: 0.04 }
    ]
  }, Fuse.parseIndex(payload.fuseIndex));

  cachedPayload = payload;
  cachedFuse = fuse;
  return { payload, fuse };
}

function limitText(text, length = 360) {
  if (!text || text.length <= length) return text || '';
  return `${text.slice(0, length).trim()}...`;
}

export function getIndexPayload(options = {}) {
  return loadPayload(options).payload;
}

export function rebuildIndex() {
  return loadPayload({ rebuild: true }).payload;
}

export function searchChunks(query, options = {}) {
  const { payload, fuse } = loadPayload();
  const limit = Math.min(Number(options.limit || 10), 50);
  const includeText = options.includeText !== false;

  if (!query || !query.trim()) {
    return {
      query: query || '',
      total: 0,
      results: []
    };
  }

  const results = fuse.search(query.trim(), { limit }).map((result) => ({
    score: result.score,
    ...result.item,
    text: includeText ? limitText(result.item.text, Number(options.textLength || 600)) : undefined
  }));

  return {
    query,
    generatedAt: payload.generatedAt,
    total: results.length,
    results
  };
}

export function searchPages(query, options = {}) {
  const chunkResults = searchChunks(query, { ...options, limit: Math.min(Number(options.limit || 20) * 3, 80) });
  const byPath = new Map();

  for (const result of chunkResults.results) {
    const old = byPath.get(result.path);
    if (!old || result.score < old.score) {
      byPath.set(result.path, result);
    }
  }

  const pages = [...byPath.values()]
    .sort((a, b) => a.score - b.score)
    .slice(0, Math.min(Number(options.limit || 10), 50))
    .map((result) => ({
      score: result.score,
      path: result.path,
      url: result.url,
      title: result.title,
      heading: result.heading,
      excerpt: result.text,
      navTrail: result.navTrail,
      visible: result.visible,
      source: result.source
    }));

  return {
    query,
    generatedAt: chunkResults.generatedAt,
    total: pages.length,
    results: pages
  };
}

export function getPage(path) {
  const { payload } = loadPayload();
  const page = payload.pages.find((item) => item.path === path);
  if (!page) return null;

  const fullPath = `${bookDir}/${page.path}`;
  return {
    ...page,
    markdown: fs.existsSync(fullPath) ? fs.readFileSync(fullPath, 'utf8') : ''
  };
}
