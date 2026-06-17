import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import Markdown from '@rbook/markdown';
import { bookDir, configPath } from '@rbook/core/paths';
import { loadPageDocument } from '@rbook/search/markdownText';

export function loadBookConfig() {
  return (yaml.load(fs.readFileSync(configPath, 'utf8')) || {}) as Record<string, unknown>;
}

function stripHtml(html: string) {
  return html
    .replace(/<script[\s\S]*?<\/script>/gi, ' ')
    .replace(/<style[\s\S]*?<\/style>/gi, ' ')
    .replace(/<[^>]+>/g, ' ')
    .replace(/&nbsp;/g, ' ')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&amp;/g, '&')
    .replace(/\s+/g, ' ')
    .trim();
}

function buildTocNode(item: any, pagesByPath: Map<string, any>, basePath = '', trail: string[] = []) {
  if (!item || !item.title) return null;

  const type = item.type || 'page';
  const nextTrail = type === 'info' ? trail : [...trail, item.title].filter(Boolean);
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
    title: item.title,
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

export function createPagePayload(page: any) {
  const fullPath = path.join(bookDir, page.path);
  const document = loadPageDocument(page);
  const renderer = new Markdown(fullPath);
  const rendered = renderer.toJSON();

  return {
    ...page,
    title: document.title,
    url: document.url,
    frontMatter: document.frontMatter,
    headings: document.headings,
    excerpt: document.excerpt,
    markdown: rendered.md_content || page.markdown || '',
    html: rendered.html_content || '',
    text: document.text || stripHtml(rendered.html_content || ''),
    chunks: document.chunks
  };
}
