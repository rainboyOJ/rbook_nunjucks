import fs from 'fs';
import { buildSearchIndex, SEARCH_INDEX_VERSION } from './buildIndex.js';
import { bookDir, searchIndexPath } from './paths.js';

let cachedPayload: any = null;

function loadPayload({ rebuild = false }: { rebuild?: boolean } = {}) {
  if (!rebuild && cachedPayload) {
    return cachedPayload;
  }

  let payload;
  if (!rebuild && fs.existsSync(searchIndexPath)) {
    payload = JSON.parse(fs.readFileSync(searchIndexPath, 'utf8'));
    if (payload.version !== SEARCH_INDEX_VERSION) {
      payload = buildSearchIndex();
    }
  } else {
    payload = buildSearchIndex();
  }

  cachedPayload = payload;
  return payload;
}

export function getIndexPayload(options: { rebuild?: boolean } = {}) {
  return loadPayload(options);
}

export function rebuildIndex() {
  return loadPayload({ rebuild: true });
}

export function getPage(path: string) {
  const payload = loadPayload();
  const page = payload.pages.find((item: any) => item.path === path);
  if (!page) return null;

  const fullPath = `${bookDir}/${page.path}`;
  return {
    ...page,
    markdown: fs.existsSync(fullPath) ? fs.readFileSync(fullPath, 'utf8') : ''
  };
}

export function getPageById(id: string) {
  const payload = loadPayload();
  const page = payload.pages.find((item: any) => item.id === id);
  if (!page) return null;

  const fullPath = `${bookDir}/${page.path}`;
  return {
    ...page,
    markdown: fs.existsSync(fullPath) ? fs.readFileSync(fullPath, 'utf8') : ''
  };
}

export function getCodeById(id: string) {
  const payload = loadPayload();
  const codes = payload.codes || [];
  return codes.find((item: any) => item.id === id) || null;
}
