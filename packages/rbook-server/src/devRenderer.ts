import fs from 'fs';
import path from 'path';
import { spawnSync } from 'child_process';
import rbook from '@rbook/core';
import {
  appDir,
  bookDir,
  configPath,
  distDir,
  publicDir
} from '@rbook/core/paths';
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
  return null;
}

interface DotCacheEntry {
  mtimeMs: number;
  body: Buffer;
}

export class DevRenderer {
  private book: rbook | null = null;
  private configMtimeMs = -1;
  private dotAvailable: boolean | null = null;
  private dotCache = new Map<string, DotCacheEntry>();

  private refreshBook() {
    const mtimeMs = fs.existsSync(configPath) ? fs.statSync(configPath).mtimeMs : -1;
    if (!this.book || mtimeMs !== this.configMtimeMs) {
      this.book = new rbook();
      this.configMtimeMs = mtimeMs;
    }

    // Render the menu each request so edits to menu.pug appear after refresh.
    (this.book.config as Record<string, unknown>).menuHtml = this.book.renderMenu();
    return this.book;
  }

  private pageResponse(pathname: string): DevResponse | null {
    const page = pagePathForUrl(pathname);
    if (!page) return this.notFound(pathname);

    const book = this.refreshBook();
    const sourcePath = inside(bookDir, page.relativePath);
    if (!sourcePath || !fs.existsSync(sourcePath) || !fs.statSync(sourcePath).isFile()) {
      // Let static app pages such as /code_template/index.html keep working.
      const staticPath = inside(distDir, pathname.replace(/^\/+/, ''));
      if (staticPath && fs.existsSync(staticPath) && fs.statSync(staticPath).isFile()) return null;
      return this.notFound(pathname, path.join(bookDir, page.relativePath));
    }

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
    const source = sourcePath ? `<p>源文件：<code>${escapeHtml(sourcePath)}</code></p>` : '';
    return {
      statusCode: 500,
      contentType: 'text/html; charset=utf-8',
      body: `<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><title>开发渲染错误</title><style>body{font-family:system-ui,sans-serif;line-height:1.5;margin:2rem}pre{white-space:pre-wrap;background:#f6f8fa;padding:1rem}</style></head><body><h1>开发渲染错误</h1><p>请求：<code>${escapeHtml(requestUrl)}</code></p>${source}<pre>${escapeHtml(detail)}</pre></body></html>`
    };
  }
}

export default DevRenderer;
