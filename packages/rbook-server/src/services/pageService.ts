import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import Markdown from '@rbook/markdown';
import { expandIncludeCode } from '@rbook/markdown/include-code';
import { bookDir, codeTemplateDir, configPath, contentDir } from '@rbook/core/paths';
import { loadPageDocument } from '@rbook/search/markdownText';

export function loadBookConfig() {
  return (yaml.load(fs.readFileSync(configPath, 'utf8')) || {}) as Record<string, unknown>;
}

function buildTocNode(item: any, pagesByPath: Map<string, any>, basePath = '', trail: string[] = []) {
  if (!item || !item.title) return null;

  const type = item.type || 'page';
  const displayTitle = item['english-title']
    ? `[${item['english-title']}] ${item.title}`
    : item.title;
  const nextTrail = type === 'info' ? trail : [...trail, displayTitle].filter(Boolean);
  const rawPath = item.path ? path.posix.join(basePath, item.path) : '';
  const sections = Array.isArray(item.sections)
    ? item.sections
        .map((section: any) => buildTocNode(section, pagesByPath, rawPath, nextTrail))
        .filter(Boolean)
    : [];

  const candidates = [
    rawPath,
    `${rawPath}.md`,
    path.posix.join(rawPath, 'index.md')
  ].filter(Boolean);
  const page = candidates.map((candidate) => pagesByPath.get(candidate)).find(Boolean);

  return {
    title: displayTitle,
    type,
    path: page?.path || null,
    url: page?.url || null,
    navTrail: page?.navTrail || nextTrail,
    visible: page?.visible ?? type !== 'info',
    children: sections
  };
}

export function buildToc(index: any) {
  const pagesByPath = new Map<string, any>(index.pages.map((page: any) => [page.path, page]));
  const config = loadBookConfig();
  const chapters = Array.isArray(config.chapters) ? config.chapters : [];
  return chapters
    .map((chapter: any) => buildTocNode(chapter, pagesByPath))
    .filter(Boolean);
}

export function createPagePayload(page: any, codes: any[] = []) {
  const fullPath = path.join(bookDir, page.path);
  const document = loadPageDocument(page);
  const codesById = new Map(codes.map((code) => [code.id, code]));
  const resolveCodeId = (id: string) => codesById.get(id) || null;
  const markdown = new Markdown(fullPath, {
    baseDir: contentDir,
    codeDir: codeTemplateDir,
    resolveCodeId
  });
  const source = markdown.source_content || page.markdown || markdown.md_content || '';

  return {
    ...page,
    title: document.title,
    url: document.url,
    frontMatter: document.frontMatter,
    headings: document.headings,
    markdown: expandIncludeCode(source, {
      baseDir: contentDir,
      codeDir: codeTemplateDir,
      currentFilePath: fullPath,
      resolveCodeId
    })
  };
}
