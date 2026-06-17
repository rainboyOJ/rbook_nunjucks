import fs from 'fs';
import path from 'path';
import { codeTemplateDir } from '@rbook/core/paths';
import { buildHref } from '../http/query.js';

export function normalizeCodeUrl(codePath: string | undefined) {
  if (!codePath || typeof codePath !== 'string') return null;
  const trimmed = codePath.trim();
  if (!trimmed) return null;

  if (trimmed.startsWith('/code/')) return trimmed.replace(/\\/g, '/');
  if (trimmed.startsWith('code/')) return `/${trimmed.replace(/\\/g, '/')}`;
  return null;
}

export function getLanguageFromPath(filePath: string) {
  const ext = path.extname(filePath).replace(/^\./, '');
  return ext || 'text';
}

export function resolveCodeFile(codePath: string | undefined) {
  const codeUrl = normalizeCodeUrl(codePath);
  if (!codeUrl) return null;

  const relativePath = codeUrl.replace(/^\/code\/?/, '');
  const absolutePath = path.resolve(codeTemplateDir, relativePath);
  const codeRoot = path.resolve(codeTemplateDir);
  const relativeToRoot = path.relative(codeRoot, absolutePath);

  // 所有对外暴露的代码读取都必须限制在 book/code 目录内。
  if (relativeToRoot.startsWith('..') || path.isAbsolute(relativeToRoot)) return null;

  return {
    path: codeUrl,
    url: codeUrl,
    absolutePath
  };
}

export function readCodePayload(codePath: string | undefined, baseUrl: string, includeContent = false) {
  const resolved = resolveCodeFile(codePath);
  if (!resolved) return null;

  const payload: Record<string, unknown> = {
    path: resolved.path,
    url: resolved.url,
    href: buildHref(baseUrl, resolved.url),
    language: getLanguageFromPath(resolved.path)
  };

  if (includeContent) {
    if (fs.existsSync(resolved.absolutePath)) {
      payload.content = fs.readFileSync(resolved.absolutePath, 'utf8');
    } else {
      payload.error = `code file not found: ${resolved.path}`;
    }
  }

  return payload;
}
