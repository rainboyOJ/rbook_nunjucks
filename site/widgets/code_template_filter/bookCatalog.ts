import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import { globSync } from 'glob';
import {
  appDir,
  bookDir,
  codeTemplateDir,
  configPath,
  themeDir
} from '@rbook/core/paths';

interface BookChapter {
  path?: string;
  sections?: BookChapter[];
}

interface BookConfig {
  chapters?: BookChapter[];
  glob?: string[];
  [key: string]: unknown;
}

export const __workdir = appDir;
export const __bookdir = bookDir;
export const __code_template_dir = codeTemplateDir;
export const __themedir = themeDir;

export function loadBookConfig(): BookConfig {
  try {
    if (!fs.existsSync(configPath)) {
      throw new Error(`配置文件不存在: ${configPath}`);
    }

    const content = fs.readFileSync(configPath, 'utf8');
    return yaml.load(content) as BookConfig;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`加载配置文件失败: ${message}`);
  }
}

function findMarkdownFile(basePath: string, relativePath: string) {
  const fullPath = path.join(__bookdir, basePath, relativePath);

  if (fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
    const indexPath = path.join(fullPath, 'index.md');
    return fs.existsSync(indexPath)
      ? path.join(basePath, relativePath, 'index.md')
      : null;
  }

  if (fullPath.endsWith('.md') && fs.existsSync(fullPath)) {
    return path.join(basePath, relativePath);
  }

  const mdPath = `${fullPath}.md`;
  return fs.existsSync(mdPath)
    ? path.join(basePath, `${relativePath}.md`)
    : null;
}

function collectChapterMarkdownFiles(chapters: BookChapter[] = [], basePath = '') {
  const files: string[] = [];

  for (const chapter of chapters) {
    if (!chapter.path) continue;

    if (Array.isArray(chapter.sections)) {
      const childBasePath = basePath ? path.join(basePath, chapter.path) : chapter.path;
      files.push(...collectChapterMarkdownFiles(chapter.sections, childBasePath));
      continue;
    }

    const foundFile = findMarkdownFile(basePath, chapter.path);
    if (foundFile) files.push(foundFile);
  }

  return files;
}

export function collectMarkdownFiles(config = loadBookConfig()) {
  const files = new Set(collectChapterMarkdownFiles(config.chapters));

  // glob 中的文章没有出现在首页目录里，但代码模板页面仍然需要能检索到它们。
  for (const pattern of config.glob || []) {
    for (const mdFile of globSync(pattern, {
      cwd: __bookdir,
      nodir: true,
      ignore: ['node_modules/**']
    })) {
      if (mdFile.endsWith('.md')) files.add(mdFile);
    }
  }

  return [...files];
}
