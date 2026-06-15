import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import { globSync } from 'glob';
import { bookDir, configPath as defaultConfigPath, toBookPath, toPosixPath } from './paths.js';
import type { BookChapter, BookConfig, CollectedPage, CollectPagesOptions, PageSource } from './types.js';

const ignoredNamePatterns = [
  /(^|\/)\./,
  /(^|\/)node_modules\//,
  /(^|\/)草稿/,
  /(^|\/)TODO\.md$/i,
  /\.bak$/i,
  /\.backup$/i,
  /copy\.md$/i
];

function readConfig(configPath = defaultConfigPath): BookConfig {
  const raw = fs.readFileSync(configPath, 'utf8');
  return (yaml.load(raw) || {}) as BookConfig;
}

function shouldIndexBookPath(bookPath: string) {
  return !ignoredNamePatterns.some((pattern) => pattern.test(bookPath));
}

function checkMarkdownFile(basePath: string, relativePath: string) {
  const fullPath = path.join(bookDir, basePath, relativePath);

  if (fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
    const indexPath = path.join(fullPath, 'index.md');
    if (fs.existsSync(indexPath)) {
      return toPosixPath(path.join(basePath, relativePath, 'index.md'));
    }
  }

  if (fullPath.endsWith('.md') && fs.existsSync(fullPath)) {
    return toPosixPath(path.join(basePath, relativePath));
  }

  const mdPath = `${fullPath}.md`;
  if (fs.existsSync(mdPath)) {
    return toPosixPath(path.join(basePath, `${relativePath}.md`));
  }

  return null;
}

function collectFromChapters(
  chapters: BookChapter[] | undefined,
  basePath = '',
  pages: CollectedPage[] = [],
  trail: string[] = []
) {
  if (!Array.isArray(chapters)) return pages;

  for (const item of chapters) {
    if (!item || !item.path) continue;

    const nextTrail = item.type === 'info' ? trail : [...trail, item.title].filter(Boolean);

    if (Array.isArray(item.sections) && item.sections.length > 0) {
      const subPath = basePath ? path.join(basePath, item.path) : item.path;
      collectFromChapters(item.sections, subPath, pages, nextTrail);
      continue;
    }

    const filePath = checkMarkdownFile(basePath, item.path);
    if (!filePath) continue;

    pages.push({
      source: 'chapters' as PageSource,
      visible: true,
      path: filePath,
      title: item.title || '',
      navTrail: nextTrail
    });
  }

  return pages;
}

function collectFromGlob(config: BookConfig) {
  const pages: CollectedPage[] = [];
  for (const pattern of config.glob || []) {
    const matches = globSync(pattern, {
      cwd: bookDir,
      nodir: true,
      absolute: true,
      ignore: ['**/node_modules/**']
    });

    for (const filePath of matches) {
      if (!filePath.endsWith('.md')) continue;
      pages.push({
        source: 'glob',
        visible: false,
        path: toBookPath(filePath),
        title: '',
        navTrail: []
      });
    }
  }
  return pages;
}

function collectAllMarkdownFiles() {
  return globSync('**/*.md', {
    cwd: bookDir,
    nodir: true,
    absolute: false,
    follow: true,
    ignore: ['**/node_modules/**']
  }).map(toPosixPath);
}

export function collectPages(options: CollectPagesOptions = {}) {
  const config = readConfig(options.configPath);
  const byPath = new Map<string, CollectedPage>();

  const addPage = (page: CollectedPage) => {
    if (!page.path || !shouldIndexBookPath(page.path)) return;
    const old = byPath.get(page.path);
    if (!old) {
      byPath.set(page.path, page);
      return;
    }

    const source = old.source === 'all' && page.source !== 'all' ? page.source : old.source;
    byPath.set(page.path, {
      ...old,
      ...page,
      source,
      visible: old.visible || page.visible,
      title: old.title || page.title,
      navTrail: old.navTrail?.length ? old.navTrail : page.navTrail
    });
  };

  addPage({
    source: 'index',
    visible: true,
    path: 'index.md',
    title: config.title || '首页',
    navTrail: [config.title || '首页']
  });

  if (fs.existsSync(path.join(bookDir, 'about.md'))) {
    addPage({
      source: 'about',
      visible: true,
      path: 'about.md',
      title: '关于',
      navTrail: ['关于']
    });
  }

  collectFromChapters(config.chapters).forEach(addPage);
  collectFromGlob(config).forEach(addPage);

  if (options.includeAllMarkdown !== false) {
    for (const filePath of collectAllMarkdownFiles()) {
      addPage({
        source: 'all',
        visible: false,
        path: filePath,
        title: '',
        navTrail: []
      });
    }
  }

  return {
    site: config,
    pages: [...byPath.values()].sort((a, b) => a.path.localeCompare(b.path, 'zh-CN'))
  };
}
