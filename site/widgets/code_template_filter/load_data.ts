import fs from 'fs';
import path from 'path';
import fse from 'fs-extra';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import type { Plugin } from 'vite';
import {
  loadCodeConfig,
  validateCodes,
  type CodeTemplateItem
} from '@rbook/core';
import {
  __code_template_dir,
  __bookdir,
  collectMarkdownFiles
} from './bookCatalog.js';
import matter from 'gray-matter';

interface ArticleInfo {
  id: string;
  title: string;
  url: string;
}

interface CodeTemplate {
  id: string;
  title?: string;
  tags?: string[];
  code: string;
  desc?: string;
  language?: string;
  articles?: ArticleInfo[];
  [key: string]: unknown;
}

const __dirname = dirname(fileURLToPath(import.meta.url));
const template_array: CodeTemplate[] = [];

function loadCodeYaml(): CodeTemplateItem[] {
  const config = loadCodeConfig({ strict: true });
  const errors = validateCodes(config.codes).filter((item) => item.level === 'ERROR');
  if (errors.length > 0) {
    const details = errors
      .map((item) => `${item.filePath}: ${item.message}`)
      .join('\n');
    throw new Error(`invalid code template configuration:\n${details}`);
  }
  return config.codes;
}

function pathEscapesRoot(relativePath: string) {
  return relativePath === '..'
    || relativePath.startsWith(`..${path.sep}`)
    || path.isAbsolute(relativePath);
}

function copyTemplateCode(codePath: string) {
  if (typeof codePath !== 'string' || codePath.length === 0) {
    throw new Error('code template path must be a non-empty string');
  }

  const codeRoot = path.resolve(__code_template_dir);
  const sourcePath = path.resolve(codeRoot, codePath);
  const relativePath = path.relative(codeRoot, sourcePath);
  if (!relativePath || pathEscapesRoot(relativePath)) {
    throw new Error(`code template path escapes book/code: ${codePath}`);
  }
  if (!fs.existsSync(sourcePath) || !fs.statSync(sourcePath).isFile()) {
    throw new Error(`code template file does not exist: ${sourcePath}`);
  }

  const realCodeRoot = fs.realpathSync(codeRoot);
  const realSourcePath = fs.realpathSync(sourcePath);
  const realRelativePath = path.relative(realCodeRoot, realSourcePath);
  if (!realRelativePath || pathEscapesRoot(realRelativePath)) {
    throw new Error(`code template path resolves outside book/code: ${codePath}`);
  }

  const publicCodePath = path.posix.join(
    'code',
    relativePath.split(path.sep).join('/')
  );
  const publicRoot = path.resolve(__dirname, 'public');
  const targetPath = path.resolve(publicRoot, ...publicCodePath.split('/'));
  const targetRelativePath = path.relative(publicRoot, targetPath);
  if (!targetRelativePath || pathEscapesRoot(targetRelativePath)) {
    throw new Error(`invalid public code path: ${publicCodePath}`);
  }

  fse.copySync(realSourcePath, targetPath);
  return publicCodePath;
}

async function loadTemplateRecords() {
  template_array.length = 0;
  const rawCodes = loadCodeYaml();

  // 构建映射：codeId -> 引用该 code 的文章
  const codeToArticles: Record<string, ArticleInfo[]> = {};
  for (const c of rawCodes) {
    codeToArticles[c.id] = [];
  }

  for (const mdFile of collectMarkdownFiles()) {
    const mdPath = path.join(__bookdir, mdFile);
    if (!fs.existsSync(mdPath)) continue;
    try {
      const raw = fs.readFileSync(mdPath, 'utf8');
      const fm = matter(raw).data as any;
      if (!fm) continue;
      const refs = Array.isArray(fm.code_template) ? fm.code_template : [];
      const articleId = fm.id || '';
      const articleTitle = fm.title || mdFile;
      const articleUrl = '/' + mdFile.replace(/\.md$/, '.html').replace(/\\/g, '/');

      for (const refId of refs) {
        if (typeof refId === 'string' && codeToArticles[refId]) {
          codeToArticles[refId].push({
            id: articleId,
            title: articleTitle,
            url: articleUrl
          });
        }
      }
    } catch {
      // 忽略读取错误
    }
  }

  for (const item of rawCodes) {
    const publicCodePath = copyTemplateCode(item.path);

    template_array.push({
      id: item.id,
      title: item.description || item.id,
      desc: item.description || '',
      tags: Array.isArray(item.tags) ? item.tags : [],
      code: publicCodePath,
      language: item.language || 'cpp',
      articles: codeToArticles[item.id] || []
    });
  }
}

export default async function nodejsPlugin() {
  await loadTemplateRecords();

  return {
    name: 'nodejs-loadBook-templateCode-plugin',
    config() {
      return {
        define: {
          template_array
        }
      };
    }
  } satisfies Plugin;
}
