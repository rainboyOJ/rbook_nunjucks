import fs from 'fs';
import path from 'path';
import fse from 'fs-extra';
import matter from 'gray-matter';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import type { Plugin } from 'vite';
import {
  __bookdir,
  __code_template_dir,
  __workdir,
  collectMarkdownFiles
} from './bookCatalog.js';

// 这个 Vite 插件在构建代码模板页面前运行：
// 1. 扫描所有文章 front matter 中的 code_template；
// 2. 把模板代码复制到当前 widget 的 public 目录；
// 3. 注入前端需要的 template_array 全局变量。
interface CodeTemplate {
  title?: string;
  tags?: string[];
  code: string;
  desc?: string;
  sh?: string;
  [key: string]: unknown;
}

type ArticleFrontMatter = Record<string, unknown> & {
  code_template?: CodeTemplate[];
};

interface TemplateRecord extends CodeTemplate {
  front_matter: ArticleFrontMatter;
  md_path: string;
}

const __dirname = dirname(fileURLToPath(import.meta.url));
const template_array: TemplateRecord[] = [];

// 这里只读取 front matter，不渲染 Markdown。代码模板页只关心文章声明了哪些模板代码。
function readFrontMatter(mdPath: string): ArticleFrontMatter {
  try {
    const rawMarkdown = fs.readFileSync(mdPath, 'utf8');
    return matter(rawMarkdown).data as ArticleFrontMatter;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`解析 Markdown front matter 失败: ${mdPath}\n${message}`);
  }
}

function getArticleTemplates(frontMatter: ArticleFrontMatter): CodeTemplate[] {
  return Array.isArray(frontMatter.code_template)
    ? frontMatter.code_template.filter((item): item is CodeTemplate => typeof item?.code === 'string')
    : [];
}

function resolveTemplateCodePath(mdPath: string, template: CodeTemplate) {
  // /code/... 是本项目模板代码的绝对约定路径，实际文件在 book/code 目录下。
  if (path.isAbsolute(template.code)) {
    return path.join(__code_template_dir, template.code.replace(/^\/code\/?/, ''));
  }

  // 相对路径按“声明它的文章所在目录”解析，方便局部文章引用局部代码。
  return path.join(path.dirname(mdPath), template.code);
}

function toPublicCodePath(codePath: string) {
  // book/code 下的代码发布成 /code_template/code/...，这样 URL 与文章里的 /code/... 约定接近。
  if (codePath.startsWith(__code_template_dir)) {
    return path.join('code', path.relative(__code_template_dir, codePath));
  }

  return path.relative(__workdir, codePath);
}

function copyTemplateCode(codePath: string, publicCodePath: string) {
  const targetPath = path.join(__dirname, 'public', publicCodePath);
  fse.copySync(codePath, targetPath);
}

function createTemplateRecord(
  mdPath: string,
  frontMatter: ArticleFrontMatter,
  template: CodeTemplate
): TemplateRecord {
  const codePath = resolveTemplateCodePath(mdPath, template);
  const publicCodePath = toPublicCodePath(codePath);

  copyTemplateCode(codePath, publicCodePath);

  return {
    ...template,
    code: publicCodePath,
    front_matter: frontMatter,
    md_path: mdPath
  };
}

async function loadTemplateRecords() {
  template_array.length = 0;

  for (const mdFile of collectMarkdownFiles()) {
    const mdPath = path.join(__bookdir, mdFile);
    if (!fs.existsSync(mdPath)) continue;

    const frontMatter = readFrontMatter(mdPath);
    const templates = getArticleTemplates(frontMatter);
    if (templates.length === 0) continue;

    for (const template of templates) {
      template_array.push(createTemplateRecord(mdPath, frontMatter, template));
    }
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
