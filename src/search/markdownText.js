import fs from 'fs';
import path from 'path';
import matter from 'gray-matter';
import { bookDir, bookPathToUrl } from './paths.js';

const includeRegex = /^@include_md\("([^"]+)"\)\s*$/gm;

function readMarkdownWithIncludes(filePath, visited = new Set()) {
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

function stripMarkdownNoise(content) {
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

function titleFromMarkdown(content, fallback) {
  const heading = content.match(/^#\s+(.+)$/m);
  return heading ? heading[1].trim() : fallback;
}

function splitByHeading(content) {
  const lines = content.split(/\r?\n/);
  const sections = [];
  let current = { heading: '', level: 0, lines: [] };

  for (const line of lines) {
    const match = line.match(/^(#{1,4})\s+(.+?)\s*#*\s*$/);
    if (match && current.lines.length > 0) {
      sections.push(current);
      current = { heading: match[2].trim(), level: match[1].length, lines: [line] };
      continue;
    }
    if (match && current.lines.length === 0) {
      current.heading = match[2].trim();
      current.level = match[1].length;
    }
    current.lines.push(line);
  }

  if (current.lines.length > 0) sections.push(current);
  return sections;
}

function splitLongText(text, maxLength = 1200) {
  if (text.length <= maxLength) return [text];

  const parts = [];
  let start = 0;
  while (start < text.length) {
    let end = Math.min(start + maxLength, text.length);
    const boundary = text.lastIndexOf('。', end);
    if (boundary > start + 300) end = boundary + 1;
    parts.push(text.slice(start, end).trim());
    start = end;
  }
  return parts.filter(Boolean);
}

export function loadPageDocument(page) {
  const fullPath = path.join(bookDir, page.path);
  const raw = readMarkdownWithIncludes(fullPath);
  const parsed = matter(raw);
  const title = parsed.data.title || page.title || titleFromMarkdown(parsed.content, page.path);
  const text = stripMarkdownNoise(parsed.content);
  const sections = splitByHeading(parsed.content);
  const headings = sections.map((section) => section.heading).filter(Boolean);
  const chunks = [];

  for (const section of sections) {
    const sectionText = stripMarkdownNoise(section.lines.join('\n'));
    for (const [index, part] of splitLongText(sectionText).entries()) {
      if (!part) continue;
      chunks.push({
        id: `${page.path}#${chunks.length}`,
        path: page.path,
        url: bookPathToUrl(page.path),
        title,
        heading: section.heading || title,
        headingLevel: section.level || 1,
        chunkIndex: chunks.length,
        splitIndex: index,
        text: part
      });
    }
  }

  if (chunks.length === 0 && text) {
    chunks.push({
      id: `${page.path}#0`,
      path: page.path,
      url: bookPathToUrl(page.path),
      title,
      heading: title,
      headingLevel: 1,
      chunkIndex: 0,
      splitIndex: 0,
      text
    });
  }

  return {
    ...page,
    title,
    url: bookPathToUrl(page.path),
    frontMatter: parsed.data,
    headings,
    text,
    excerpt: text.slice(0, 240),
    chunks
  };
}
