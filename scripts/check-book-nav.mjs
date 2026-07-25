#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import yaml from 'js-yaml';
import { globSync } from 'glob';

const repoDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const pagesDir = path.join(repoDir, 'book/pages');
const configPath = path.join(repoDir, 'book/book.yaml');

function toPosix(value) {
  return value.split(path.sep).join('/');
}

function readConfig() {
  return yaml.load(fs.readFileSync(configPath, 'utf8')) || {};
}

function resolvePage(basePath, rawPath) {
  if (typeof rawPath !== 'string' || !rawPath) return null;

  const relativePath = path.posix.normalize(path.posix.join(basePath, rawPath));
  if (!relativePath || relativePath === '.' || relativePath.startsWith('../')) return null;

  const fullPath = path.resolve(pagesDir, relativePath);
  const pagesRoot = path.resolve(pagesDir);
  if (fullPath !== pagesRoot && !fullPath.startsWith(`${pagesRoot}${path.sep}`)) return null;

  if (fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
    const indexPath = path.join(fullPath, 'index.md');
    return fs.existsSync(indexPath) ? toPosix(path.relative(pagesDir, indexPath)) : null;
  }

  if (fullPath.endsWith('.md') && fs.existsSync(fullPath) && fs.statSync(fullPath).isFile()) {
    return toPosix(path.relative(pagesDir, fullPath));
  }

  const markdownPath = `${fullPath}.md`;
  return fs.existsSync(markdownPath) && fs.statSync(markdownPath).isFile()
    ? toPosix(path.relative(pagesDir, markdownPath))
    : null;
}

function collectChapterEntries(
  chapters,
  basePath = '',
  state = { entries: [], missingTitles: [] }
) {
  if (!Array.isArray(chapters)) return state;

  for (const item of chapters) {
    if (!item || typeof item !== 'object') continue;
    const rawPath = typeof item.path === 'string' ? item.path : '';
    const title = typeof item.title === 'string' ? item.title.trim() : '';
    if (!title) state.missingTitles.push(rawPath || basePath || '<root>');

    if (Array.isArray(item.sections)) {
      const childBasePath = rawPath ? path.posix.join(basePath, rawPath) : basePath;
      collectChapterEntries(item.sections, childBasePath, state);
      continue;
    }

    // Info separators and folder labels do not point to a page.
    if (!rawPath) continue;

    state.entries.push({
      path: resolvePage(basePath, rawPath),
      rawPath,
      title,
      item
    });
  }

  return state;
}

function expandGlobFiles(patterns) {
  const files = new Set();
  for (const pattern of patterns || []) {
    for (const file of globSync(pattern, {
      cwd: pagesDir,
      nodir: true,
      ignore: ['**/node_modules/**']
    })) {
      if (file.endsWith('.md')) files.add(toPosix(file));
    }
  }
  return files;
}

function readFrontMatter(relativePath) {
  const fullPath = path.join(pagesDir, relativePath);
  const source = fs.readFileSync(fullPath, 'utf8');
  const match = source.match(/^---\r?\n([\s\S]*?)\r?\n---(?:\r?\n|$)/);
  if (!match) return {};

  try {
    const value = yaml.load(match[1]);
    return value && typeof value === 'object' ? value : {};
  } catch {
    return {};
  }
}

function printList(label, values) {
  console.log(`${label} (${values.length})`);
  for (const value of values) console.log(`  - ${value}`);
}

function main() {
  const config = readConfig();
  const { entries, missingTitles } = collectChapterEntries(config.chapters);
  const errors = missingTitles.map(value => `chapters 条目缺少 title: ${value}`);
  const warnings = [];
  const seenPaths = new Map();

  for (const entry of entries) {
    if (!entry.path) {
      errors.push(`chapters 路径不存在: ${entry.rawPath}`);
      continue;
    }

    const previous = seenPaths.get(entry.path);
    if (previous) {
      errors.push(`chapters 重复路径: ${entry.path} (${previous}、${entry.title || '未命名'})`);
    } else {
      seenPaths.set(entry.path, entry.title || entry.rawPath);
    }
  }

  const chapterPaths = new Set(entries.map(entry => entry.path).filter(Boolean));
  const globPaths = expandGlobFiles(config.glob);
  const hiddenGlobPaths = [...globPaths]
    .filter(relativePath => !chapterPaths.has(relativePath))
    .sort((a, b) => a.localeCompare(b, 'zh-CN'));

  const sourcePaths = new Set([...chapterPaths, ...globPaths]);
  const ids = new Map();
  for (const relativePath of sourcePaths) {
    const id = readFrontMatter(relativePath).id;
    if (id === undefined || id === null || id === '') continue;
    const key = String(id);
    const paths = ids.get(key) || [];
    paths.push(relativePath);
    ids.set(key, paths);
  }

  for (const [id, paths] of ids) {
    if (paths.length > 1) warnings.push(`front matter id 重复: ${id} (${paths.join('、')})`);
  }

  console.log(`[nav] chapters 条目: ${entries.length}`);
  console.log(`[nav] glob 文件: ${globPaths.size}`);
  printList('[nav] glob 中未进入目录的文件', hiddenGlobPaths);

  for (const error of errors) console.error(`[nav] ERROR ${error}`);
  for (const warning of warnings) console.warn(`[nav] WARN ${warning}`);

  if (errors.length > 0) {
    console.error(`[nav] 检查失败: ${errors.length} 个错误，${warnings.length} 个警告`);
    process.exitCode = 1;
    return;
  }

  console.log(`[nav] 检查通过: ${warnings.length} 个警告`);
}

try {
  main();
} catch (error) {
  console.error('[nav] 检查过程失败:', error instanceof Error ? error.message : error);
  process.exitCode = 1;
}
