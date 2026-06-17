import fs from 'fs';
import path from 'path';
import fse from 'fs-extra';
import matter from 'gray-matter';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import type { Plugin } from 'vite';
import rbook, {
  __bookdir,
  __code_template_dir,
  __workdir
} from './fake_rbook.js';

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
  if (path.isAbsolute(template.code)) {
    return path.join(__code_template_dir, template.code.replace(/^\/code\/?/, ''));
  }

  return path.join(path.dirname(mdPath), template.code);
}

function toPublicCodePath(codePath: string) {
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

  const book = new rbook();

  for (const mdFile of book.AllMarkdownFiles) {
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
