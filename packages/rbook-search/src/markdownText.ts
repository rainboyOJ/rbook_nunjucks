import fs from 'fs';
import path from 'path';
import matter from 'gray-matter';
import { bookDir, bookPathToUrl } from './paths.js';
import type { CollectedPage, PageDocument } from './types.js';

const includeRegex = /^@include_md\("([^"]+)"\)\s*$/gm;

function readMarkdownWithIncludes(filePath: string, visited = new Set<string>()) {
  const realPath = path.resolve(filePath);
  if (visited.has(realPath)) {
    return `<!-- skipped circular include: ${path.basename(filePath)} -->`;
  }

  visited.add(realPath);
  const raw = fs.readFileSync(realPath, 'utf8');
  const currentDir = path.dirname(realPath);

  return raw.replace(includeRegex, (_match, includePath) => {
    const fullPath = path.resolve(currentDir, includePath);
    if (!fs.existsSync(fullPath)) {
      return `<!-- missing include: ${includePath} -->`;
    }
    return readMarkdownWithIncludes(fullPath, visited);
  });
}

function stripMarkdownNoise(content: string) {
  return content
    .replace(/```[\s\S]*?```/g, (block) => block.replace(/```[^\n]*\n?|\n?```/g, '\n'))
    .replace(/~~~[\s\S]*?~~~/g, (block) => block.replace(/~~~[^\n]*\n?|\n?~~~/g, '\n'))
    .replace(/!\[[^\]]*]\([^)]+\)/g, ' ')
    .replace(/\[([^\]]+)]\([^)]+\)/g, '$1')
    .replace(/\[\[([^\]|]+)(?:\|([^\]]+))?]]/g, '$2$1')
    .replace(/<[^>]+>/g, ' ')
    .replace(/[`*_>#|~=-]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function titleFromMarkdown(content: string, fallback: string) {
  const heading = content.match(/^#\s+(.+)$/m);
  return heading ? heading[1].trim() : fallback;
}

function extractHeadings(content: string) {
  return [...content.matchAll(/^(#{1,4})\s+(.+?)\s*#*\s*$/gm)]
    .map((match) => match[2].trim());
}

export function loadPageDocument(page: CollectedPage): PageDocument {
  const fullPath = path.join(bookDir, page.path);
  const raw = readMarkdownWithIncludes(fullPath);
  const parsed = matter(raw);
  const title = String(parsed.data.title || page.title || titleFromMarkdown(parsed.content, page.path));
  const text = stripMarkdownNoise(parsed.content);
  const headings = extractHeadings(parsed.content);

  return {
    ...page,
    title,
    url: bookPathToUrl(page.path),
    frontMatter: parsed.data as Record<string, unknown>,
    headings,
    text,
    excerpt: text.slice(0, 240)
  };
}
