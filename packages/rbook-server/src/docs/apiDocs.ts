import fs from 'fs';
import { render as renderMarkdown } from '@rbook/markdown/markdown-it';
import { fromRoot, themeDir } from '@rbook/core/paths';
import { renderTemplate } from '@rbook/core/renderEngine';

export const apiDocsMarkdownPath = fromRoot('docs/api-usage.md');

export function readApiDocsMarkdown() {
  try {
    return fs.readFileSync(apiDocsMarkdownPath, 'utf8');
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`读取 API 文档失败: ${apiDocsMarkdownPath}: ${message}`);
  }
}

function normalizeBaseUrl(baseUrl: string) {
  const parsed = new URL(baseUrl);
  if (parsed.protocol !== 'http:' && parsed.protocol !== 'https:') {
    throw new Error(`不支持的 API 文档协议: ${parsed.protocol}`);
  }
  return parsed.origin;
}

export function renderApiDocsPage(baseUrl: string) {
  const markdown = readApiDocsMarkdown();
  const renderedSource = markdown.replaceAll('$BASE_URL', normalizeBaseUrl(baseUrl));
  const htmlContent = renderMarkdown(renderedSource, {
    mdit: {},
    filePath: apiDocsMarkdownPath
  });

  return renderTemplate(themeDir, 'api-docs', {
    title: 'Rbook HTTP API 使用指南',
    htmlContent
  });
}
