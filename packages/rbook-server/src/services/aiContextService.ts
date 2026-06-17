import fs from 'fs';
import path from 'path';
import { bookDir } from '@rbook/core/paths';
import {
  getLanguageFromPath,
  normalizeCodeUrl,
  readCodePayload,
  resolveCodeFile
} from './codeService.js';
import { createPagePayload } from './pageService.js';

function asRecord(value: unknown): Record<string, any> {
  return value && typeof value === 'object' && !Array.isArray(value)
    ? value as Record<string, any>
    : {};
}

function asStringArray(value: unknown) {
  return Array.isArray(value)
    ? value.filter((item) => typeof item === 'string')
    : [];
}

function getPageDescription(page: any) {
  const frontMatter = asRecord(page.frontMatter);
  return String(frontMatter.description || frontMatter.desc || page.excerpt || '').trim();
}

function createCitation(page: any) {
  return {
    title: page.title,
    path: page.path,
    url: page.url
  };
}

function getCodeTemplates(page: any, includeContent = false) {
  const frontMatter = asRecord(page.frontMatter);
  const templates = Array.isArray(frontMatter.code_template) ? frontMatter.code_template : [];

  return templates
    .map((template: any) => {
      const code = readCodePayload(template?.code, includeContent);
      if (!code) return null;
      return {
        source: 'frontMatter',
        title: template.title || '',
        desc: template.desc || '',
        tags: asStringArray(template.tags),
        ...template,
        code: code.path,
        codeUrl: code.url,
        language: code.language,
        ...(includeContent ? { content: code.content } : {})
      };
    })
    .filter(Boolean);
}

const includeCodeRegex = /^@include-code\(([^,)]+)(?:,\s*([^)]+))?\s*\)/gm;

function resolveIncludedCode(rawCodePath: string, pagePath: string) {
  const codeUrl = normalizeCodeUrl(rawCodePath);
  if (codeUrl) {
    const resolved = resolveCodeFile(codeUrl);
    return resolved ? { ...resolved, url: codeUrl } : null;
  }

  const fullPath = path.resolve(bookDir, path.dirname(pagePath), rawCodePath.trim());
  const bookRoot = path.resolve(bookDir);
  const relativeToBook = path.relative(bookRoot, fullPath);
  if (relativeToBook.startsWith('..') || path.isAbsolute(relativeToBook)) return null;

  return {
    path: rawCodePath.trim(),
    url: null,
    absolutePath: fullPath
  };
}

function getIncludedCode(pagePath: string, markdown: string, includeContent = false) {
  const included = [];
  const seen = new Set<string>();

  for (const match of markdown.matchAll(includeCodeRegex)) {
    const rawCodePath = match[1].trim();
    const language = (match[2] || '').trim();
    const resolved = resolveIncludedCode(rawCodePath, pagePath);
    if (!resolved || seen.has(resolved.absolutePath)) continue;
    seen.add(resolved.absolutePath);

    const payload: Record<string, unknown> = {
      source: 'include-code',
      path: resolved.path,
      code: rawCodePath,
      codeUrl: resolved.url,
      language: language || getLanguageFromPath(rawCodePath)
    };

    if (includeContent) {
      if (fs.existsSync(resolved.absolutePath)) {
        payload.content = fs.readFileSync(resolved.absolutePath, 'utf8');
      } else {
        payload.error = `code file not found: ${rawCodePath}`;
      }
    }

    included.push(payload);
  }

  return included;
}

export function createAiCatalogItem(page: any) {
  const frontMatter = asRecord(page.frontMatter);
  return {
    path: page.path,
    url: page.url,
    title: page.title,
    description: getPageDescription(page),
    excerpt: page.excerpt || '',
    tags: asStringArray(frontMatter.tags),
    categories: asStringArray(frontMatter.categories),
    headings: page.headings || [],
    navTrail: page.navTrail || [],
    visible: page.visible,
    source: page.source,
    codeTemplates: getCodeTemplates(page, false),
    citation: createCitation(page)
  };
}

export function createAiPageContext(page: any, options: {
  includeCode?: boolean;
  includeHtml?: boolean;
} = {}) {
  const payload = createPagePayload(page);
  const article: Record<string, unknown> = {
    path: payload.path,
    url: payload.url,
    title: payload.title,
    description: getPageDescription(payload),
    excerpt: payload.excerpt,
    visible: payload.visible,
    source: payload.source,
    navTrail: payload.navTrail || [],
    tags: asStringArray(payload.frontMatter?.tags),
    categories: asStringArray(payload.frontMatter?.categories),
    frontMatter: payload.frontMatter,
    headings: payload.headings,
    markdown: payload.markdown,
    text: payload.text,
    chunks: payload.chunks,
    citation: createCitation(payload)
  };

  if (options.includeHtml) article.html = payload.html;

  return {
    article,
    codeTemplates: getCodeTemplates(payload, Boolean(options.includeCode)),
    includedCode: getIncludedCode(payload.path, page.markdown || payload.markdown || '', Boolean(options.includeCode))
  };
}
