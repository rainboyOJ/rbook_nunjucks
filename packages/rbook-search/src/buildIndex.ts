import fs from 'fs';
import path from 'path';
import {
  loadCodeConfig,
  requireCodeId,
  requirePageId,
  validateCodeDirectory,
  validateCodes,
  validatePages,
  validateReferences
} from '@rbook/core';
import { collectPages } from './collectPages.js';
import { loadPageDocument } from './markdownText.js';
import { searchDir, searchIndexPath } from './paths.js';
import type { BuildSearchIndexOptions, PageDocument } from './types.js';
import type { CodeTemplateItem } from '@rbook/core';

export const SEARCH_INDEX_VERSION = 3;

function asStringArray(value: unknown) {
  return Array.isArray(value)
    ? value.filter((item) => typeof item === 'string')
    : [];
}

function buildIndexPayload(
  site: Record<string, unknown>,
  documents: PageDocument[],
  codeConfig: CodeTemplateItem[],
  errors: Array<{ path: string; message: string }>,
  options: BuildSearchIndexOptions = {}
) {
  const codes = (codeConfig || []).map((item) => ({
    id: requireCodeId(item),
    path: item.path,
    url: `/code/${String(item.path || '').replace(/^\/?code\//, '')}`,
    description: item.description || '',
    language: item.language || path.extname(item.path || '').replace(/^\./, '') || 'text',
    tags: asStringArray(item.tags),
    complexity: item.complexity || '',
    author: item.author || '',
    aliases: asStringArray(item.aliases)
  }));

  const codeToArticles: Record<string, Array<{ id: string; title: string; path: string; url: string }>> = {};
  for (const code of codes) {
    codeToArticles[code.id] = [];
  }

  for (const doc of documents) {
    const pageId = requirePageId(doc);
    const refs = asStringArray((doc.frontMatter as Record<string, unknown> | undefined)?.code_template);
    for (const codeId of refs) {
      codeToArticles[codeId].push({
        id: pageId,
        title: doc.title,
        path: doc.path,
        url: doc.url
      });
    }
  }

  const payload = {
    version: SEARCH_INDEX_VERSION,
    generatedAt: new Date().toISOString(),
    site: {
      title: site.title,
      author: site.author,
      description: site.description,
      github_repository: site.github_repository
    },
    stats: {
      pages: documents.length,
      codes: codes.length,
      errors: errors.length
    },
    pages: documents.map((doc) => ({
      id: requirePageId(doc),
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
    codes,
    codeToArticles,
    errors
  };

  if (options.write !== false) {
    fs.mkdirSync(searchDir, { recursive: true });
    fs.writeFileSync(options.outputPath || searchIndexPath, JSON.stringify(payload, null, 2));
  }

  return payload;
}

export function buildSearchIndexFromDocuments(
  site: Record<string, unknown>,
  documents: PageDocument[],
  codes: CodeTemplateItem[],
  options: BuildSearchIndexOptions = {}
) {
  return buildIndexPayload(site, documents, codes, [], options);
}

export function buildSearchIndex(options: BuildSearchIndexOptions = {}): any {
  const collected = collectPages(options);
  const documents: PageDocument[] = [];
  const errors: Array<{ path: string; message: string }> = [];

  for (const page of collected.pages) {
    try {
      documents.push(loadPageDocument(page));
    } catch (error) {
      errors.push({ path: page.path, message: error instanceof Error ? error.message : String(error) });
    }
  }

  if (errors.length > 0) {
    const details = errors.map((item) => `${item.path}: ${item.message}`).join('\n');
    throw new Error(`search index page loading failed:\n${details}`);
  }

  const codeConfig = loadCodeConfig({ strict: true });
  const validationErrors = [
    ...validateCodeDirectory(codeConfig.codes),
    ...validateCodes(codeConfig.codes),
    ...validatePages(documents),
    ...validateReferences(documents, codeConfig.codes)
  ].filter((item) => item.level === 'ERROR');
  if (validationErrors.length > 0) {
    const details = validationErrors
      .map((item) => `${item.filePath}: ${item.message}`)
      .join('\n');
    throw new Error(`search index validation failed:\n${details}`);
  }

  return buildIndexPayload(collected.site as Record<string, unknown>, documents, codeConfig.codes, errors, options);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const outputPath = process.argv[2] ? path.resolve(process.argv[2]) : searchIndexPath;
  const payload = buildSearchIndex({ outputPath });
  console.log(`Search index written: ${outputPath}`);
  console.log(`Pages: ${payload.stats.pages}, codes: ${payload.stats.codes}, errors: ${payload.stats.errors}`);
}
