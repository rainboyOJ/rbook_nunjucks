import fs from 'fs';
import path from 'path';
import { spawnSync } from 'child_process';
import rbook from '@rbook/core';
import {
  appDir,
  bookDir,
  distDir,
  publicDir
} from '@rbook/core/paths';
import {
  assertPreCheckContext,
  validatePageDocument,
  type PreCheckContext
} from '@rbook/search/preCheck';
import { loadPageDocument } from '@rbook/search/markdownText';
import { compileMarkdownCss } from './buildRuntime.js';

export interface DevResponse {
  statusCode: number;
  contentType: string;
  body: string | Buffer;
}

const assetExtensions = new Set([
  '.avif',
  '.css',
  '.gif',
  '.ico',
  '.jpeg',
  '.jpg',
  '.js',
  '.json',
  '.pdf',
  '.png',
  '.svg',
  '.webp'
]);

const contentTypes: Record<string, string> = {
  '.css': 'text/css; charset=utf-8',
  '.gif': 'image/gif',
  '.html': 'text/html; charset=utf-8',
  '.ico': 'image/x-icon',
  '.jpeg': 'image/jpeg',
  '.jpg': 'image/jpeg',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.pdf': 'application/pdf',
  '.png': 'image/png',
  '.svg': 'image/svg+xml',
  '.webp': 'image/webp',
  '.avif': 'image/avif'
};

function escapeHtml(value: unknown) {
  return String(value)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

function contentType(filePath: string) {
  return contentTypes[path.extname(filePath).toLowerCase()] || 'application/octet-stream';
}

function inside(baseDir: string, relativePath: string) {
  const base = path.resolve(baseDir);
  const candidate = path.resolve(base, relativePath);
  return candidate === base || candidate.startsWith(`${base}${path.sep}`) ? candidate : null;
}

function parsePath(requestUrl: string) {
  const rawPath = requestUrl.split(/[?#]/, 1)[0] || '/';
  let pathname: string;
  try {
    pathname = decodeURIComponent(rawPath);
  } catch {
    return null;
  }

  if (!pathname.startsWith('/') || pathname.includes('\0')) return null;
  const segments = pathname.split('/');
  if (segments.some(segment => segment === '..')) return null;
  return pathname;
}

function pagePathForUrl(pathname: string) {
  if (pathname === '/') return { relativePath: 'index.md', template: 'index' };

  const relative = pathname.replace(/^\/+/, '');
  if (!relative) return { relativePath: 'index.md', template: 'index' };
  if (relative.toLowerCase().endsWith('.html')) {
    return { relativePath: relative.slice(0, -'.html'.length) + '.md', template: null };
  }
  // 目录式 URL（无 .html 后缀）：把 pathname 当作目录，尝试 xxx/index.md。
  // 若该文件不存在，pageResponse 会回落到静态文件（如 /code_template 应用页）。
  return { relativePath: relative.replace(/\/+$/, '') + '/index.md', template: null };
}

interface DotCacheEntry {
  mtimeMs: number;
  body: Buffer;
}

interface PageValidationCacheEntry {
  mtimeMs: number;
  errors: string[];
}

export interface DiagnosticIssue {
  level: 'ERROR' | 'WARNING';
  filePath: string;
  message: string;
  stage: 'startup' | 'page' | 'render';
}

export interface DiagnosticsPayload {
  mode: 'development';
  generatedAt: string;
  stats: {
    pages: number;
    codes: number;
    errors: number;
    warnings: number;
  };
  issues: DiagnosticIssue[];
}

function safeDiagnosticPath(filePath: string) {
  const value = String(filePath || 'unknown').replaceAll('\\', '/');
  if (!path.isAbsolute(value)) return value;

  const relative = path.relative(process.cwd(), value).replaceAll('\\', '/');
  return relative && !relative.startsWith('../') && relative !== '..'
    ? relative
    : '[external path]';
}

export class DevRenderer {
  private book: rbook;
  private readonly pages: PreCheckContext['pages'];
  private readonly pagesByPath: Map<string, PreCheckContext['pages'][number]>;
  private readonly codes: PreCheckContext['codes'];
  private readonly pageValidationCache = new Map<string, PageValidationCacheEntry>();
  private readonly diagnostics = new Map<string, DiagnosticIssue>();
  private dotAvailable: boolean | null = null;
  private dotCache = new Map<string, DotCacheEntry>();

  constructor(context?: PreCheckContext) {
    const preCheck = context || assertPreCheckContext();
    this.pages = preCheck.pages;
    this.pagesByPath = new Map(preCheck.pages.map((page) => [page.path, page]));
    this.codes = preCheck.codes;
    this.book = new rbook({
      config: preCheck.site,
      codeTemplates: preCheck.codes
    });

    for (const warning of preCheck.result.warnings) {
      this.recordDiagnostic(warning, 'startup');
    }

    // The startup pre-check has already validated every page. Keep those
    // mtimes so an unchanged page does not need to be parsed again.
    for (const page of this.pages) {
      const sourcePath = inside(bookDir, page.path);
      if (!sourcePath || !fs.existsSync(sourcePath)) continue;
      this.pageValidationCache.set(page.path, {
        mtimeMs: fs.statSync(sourcePath).mtimeMs,
        errors: []
      });
    }
  }

  private refreshBook() {
    // Render the menu each request so edits to menu.pug appear after refresh.
    (this.book.config as Record<string, unknown>).menuHtml = this.book.renderMenu();
    return this.book;
  }

  private recordDiagnostic(
    issue: { level: 'ERROR' | 'WARNING'; filePath: string; message: string },
    stage: DiagnosticIssue['stage']
  ) {
    const diagnostic: DiagnosticIssue = {
      level: issue.level,
      filePath: safeDiagnosticPath(issue.filePath),
      message: String(issue.message),
      stage
    };
    const key = `${stage}:${diagnostic.filePath}:${diagnostic.level}:${diagnostic.message}`;
    this.diagnostics.set(key, diagnostic);
  }

  private clearPageDiagnostics(filePath: string) {
    const safePath = safeDiagnosticPath(filePath);
    for (const [key, issue] of this.diagnostics) {
      if (issue.stage === 'page' && issue.filePath === safePath) {
        this.diagnostics.delete(key);
      }
    }
  }

  getDiagnostics(): DiagnosticsPayload {
    const issues = [...this.diagnostics.values()].sort((a, b) => {
      return a.level.localeCompare(b.level)
        || a.filePath.localeCompare(b.filePath)
        || a.message.localeCompare(b.message);
    });
    return {
      mode: 'development',
      generatedAt: new Date().toISOString(),
      stats: {
        pages: this.pages.length,
        codes: this.codes.length,
        errors: issues.filter((issue) => issue.level === 'ERROR').length,
        warnings: issues.filter((issue) => issue.level === 'WARNING').length
      },
      issues
    };
  }

  private validatePage(page: PreCheckContext['pages'][number], sourcePath: string) {
    const mtimeMs = fs.statSync(sourcePath).mtimeMs;
    const cached = this.pageValidationCache.get(page.path);
    if (cached?.mtimeMs === mtimeMs) {
      if (cached.errors.length > 0) {
        throw new Error(`page pre-check failed for ${page.path}:\n${cached.errors.join('\n')}`);
      }
      return;
    }

    let document: PreCheckContext['pages'][number];
    this.clearPageDiagnostics(page.path);
    try {
      document = loadPageDocument(page);
    } catch (error) {
      const message = `failed to load page: ${error instanceof Error ? error.message : String(error)}`;
      this.recordDiagnostic({ level: 'ERROR', filePath: page.path, message }, 'page');
      this.pageValidationCache.set(page.path, { mtimeMs, errors: [message] });
      throw new Error(`page pre-check failed for ${page.path}:\n${message}`);
    }

    const issues = validatePageDocument(document, this.pages, this.codes);
    const errors = issues
      .filter((issue) => issue.level === 'ERROR')
      .map((issue) => `[${issue.level}] ${issue.filePath}: ${issue.message}`);
    const warnings = issues.filter((issue) => issue.level === 'WARNING');
    for (const issue of issues) {
      this.recordDiagnostic(issue, 'page');
    }
    for (const warning of warnings) {
      console.warn(`[${warning.level}] ${warning.filePath}: ${warning.message}`);
    }

    this.pageValidationCache.set(page.path, { mtimeMs, errors });
    if (errors.length > 0) {
      throw new Error(`page pre-check failed for ${page.path}:\n${errors.join('\n')}`);
    }
  }

  private pageResponse(pathname: string): DevResponse | null {
    const page = pagePathForUrl(pathname);
    if (!page) return this.notFound(pathname);

    const book = this.refreshBook();
    const sourcePath = inside(bookDir, page.relativePath);
    if (!sourcePath || !fs.existsSync(sourcePath) || !fs.statSync(sourcePath).isFile()) {
      // Let static app pages such as /code_template and /code_template/index.html keep working.
      const staticPath = inside(distDir, pathname.replace(/^\/+/, ''));
      if (staticPath && fs.existsSync(staticPath)) return null;
      return this.notFound(pathname, path.join(bookDir, page.relativePath));
    }

    const indexedPage = this.pagesByPath.get(page.relativePath);
    if (!indexedPage) {
      // The development server only exposes pages accepted by the startup
      // pre-check/index. Other files remain inaccessible as article routes.
      return this.notFound(pathname, sourcePath);
    }

    this.validatePage(indexedPage, sourcePath);

    const html = book.renderMarkdownFile(page.relativePath, page.template);
    if (html === null) {
      return this.notFound(pathname, sourcePath);
    }

    return {
      statusCode: 200,
      contentType: 'text/html; charset=utf-8',
      body: html
    };
  }

  private sourceAsset(pathname: string): DevResponse | null {
    if (pathname === '/markdown.css') {
      compileMarkdownCss(false);
      const cssPath = path.join(distDir, 'markdown.css');
      if (!fs.existsSync(cssPath)) return null;
      return {
        statusCode: 200,
        contentType: 'text/css; charset=utf-8',
        body: fs.readFileSync(cssPath)
      };
    }

    let baseDir: string | null = null;
    let relativePath = pathname.replace(/^\/+/, '');
    if (pathname.startsWith('/assets/')) {
      baseDir = path.join(appDir, 'theme/assets');
      relativePath = pathname.slice('/assets/'.length);
    } else if (pathname.startsWith('/')) {
      const publicCandidate = inside(publicDir, relativePath);
      if (publicCandidate && fs.existsSync(publicCandidate) && fs.statSync(publicCandidate).isFile()) {
        return {
          statusCode: 200,
          contentType: contentType(publicCandidate),
          body: fs.readFileSync(publicCandidate)
        };
      }
      baseDir = bookDir;
    }

    if (!baseDir) return null;
    const candidate = inside(baseDir, relativePath);
    if (!candidate) return null;

    if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) {
      if (!assetExtensions.has(path.extname(candidate).toLowerCase())) return null;
      return {
        statusCode: 200,
        contentType: contentType(candidate),
        body: fs.readFileSync(candidate)
      };
    }

    // Check distDir for static SPA directories (e.g. /code_template/)
    const distCandidate = inside(distDir, relativePath);
    if (distCandidate && fs.existsSync(distCandidate)) {
      if (fs.statSync(distCandidate).isFile()) {
        return { statusCode: 200, contentType: contentType(distCandidate), body: fs.readFileSync(distCandidate) };
      }
      if (fs.statSync(distCandidate).isDirectory()) {
        const indexPath = path.join(distCandidate, 'index.html');
        if (fs.existsSync(indexPath) && fs.statSync(indexPath).isFile()) {
          return { statusCode: 200, contentType: 'text/html; charset=utf-8', body: fs.readFileSync(indexPath) };
        }
      }
    }

    if (baseDir !== bookDir || path.extname(candidate).toLowerCase() !== '.svg') return null;
    const dotPath = candidate.slice(0, -'.svg'.length) + '.dot';
    if (!fs.existsSync(dotPath) || !fs.statSync(dotPath).isFile()) return null;
    if (this.dotAvailable === null) {
      const probe = spawnSync('dot', ['-V'], { stdio: 'ignore' });
      this.dotAvailable = !probe.error && probe.status === 0;
    }
    if (!this.dotAvailable) return null;

    const mtimeMs = fs.statSync(dotPath).mtimeMs;
    const cached = this.dotCache.get(dotPath);
    if (!cached || cached.mtimeMs < mtimeMs) {
      const result = spawnSync('dot', ['-Tsvg', dotPath], { encoding: 'buffer' });
      if (result.error || result.status !== 0) {
        throw new Error(`dot 渲染失败: ${dotPath}\n${result.stderr?.toString() || result.error?.message || ''}`);
      }
      this.dotCache.set(dotPath, { mtimeMs, body: result.stdout });
    }

    return {
      statusCode: 200,
      contentType: 'image/svg+xml',
      body: this.dotCache.get(dotPath)!.body
    };
  }

  render(requestUrl: string): DevResponse | null {
    const pathname = parsePath(requestUrl);
    if (!pathname) return this.notFound(requestUrl);

    if (pathname === '/markdown.css' || pathname.startsWith('/assets/')) {
      return this.sourceAsset(pathname);
    }

    const page = pagePathForUrl(pathname);
    if (page) return this.pageResponse(pathname);
    return this.sourceAsset(pathname);
  }

  notFound(requestUrl: string, sourcePath?: string): DevResponse {
    const pathname = parsePath(requestUrl) || requestUrl;
    const expected = sourcePath ? `<p>源文件：<code>${escapeHtml(sourcePath)}</code></p>` : '';
    return {
      statusCode: 404,
      contentType: 'text/html; charset=utf-8',
      body: `<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><title>404 Not Found</title></head><body><h1>404 Not Found</h1><p>找不到请求的页面：<code>${escapeHtml(pathname)}</code></p>${expected}</body></html>`
    };
  }

  error(requestUrl: string, error: unknown): DevResponse {
    const detail = error instanceof Error ? error.stack || error.message : String(error);
    const pathname = parsePath(requestUrl);
    const page = pathname ? pagePathForUrl(pathname) : null;
    const sourcePath = page ? path.join(bookDir, page.relativePath) : null;
    this.recordDiagnostic({
      level: 'ERROR',
      filePath: page?.relativePath || requestUrl,
      message: detail.split('\n', 1)[0]
    }, 'render');
    const source = sourcePath ? `<p>源文件：<code>${escapeHtml(sourcePath)}</code></p>` : '';
    return {
      statusCode: 500,
      contentType: 'text/html; charset=utf-8',
      body: `<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><title>开发渲染错误</title><style>body{font-family:system-ui,sans-serif;line-height:1.5;margin:2rem}pre{white-space:pre-wrap;background:#f6f8fa;padding:1rem}</style></head><body><h1>开发渲染错误</h1><p>请求：<code>${escapeHtml(requestUrl)}</code></p>${source}<pre>${escapeHtml(detail)}</pre></body></html>`
    };
  }
}

export default DevRenderer;
