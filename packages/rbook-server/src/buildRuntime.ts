import fs from 'fs';
import path from 'path';
import rbook from '@rbook/core';
import {
  appDir,
  bookDir,
  distDir,
  publicDir,
  runtimeDir
} from '@rbook/core/paths';
import { buildSearchIndex } from '@rbook/search/buildIndex';
import { assertPreCheck } from '@rbook/search/preCheck';
import { hasCommand, runCommand } from './runtimeBuild/commands.js';
import { copyIfExists, walkFiles } from './runtimeBuild/files.js';

const assetExtensions = new Set([
  '.avif',
  '.gif',
  '.ico',
  '.jpeg',
  '.jpg',
  '.pdf',
  '.png',
  '.svg',
  '.webp'
]);

export const staticWidgetApps = [
  { source: 'code_template_filter', target: 'code_template' },
  { source: 'explore', target: 'explore' },
  { source: 'article_inspector', target: 'article_inspector' },
  { source: 'tags', target: 'tags' },
  { source: 'relations', target: 'relations' },
  { source: 'practice', target: 'practice' },
  { source: 'diagnostics', target: 'diagnostics' }
] as const;

function resetRuntimeDir() {
  if (!runtimeDir) {
    fs.rmSync(distDir, { recursive: true, force: true });
    return;
  }

  fs.rmSync(runtimeDir, { recursive: true, force: true });
  fs.mkdirSync(runtimeDir, { recursive: true });
}

export function copyBookAssets() {
  if (!fs.existsSync(bookDir)) {
    throw new Error(`book directory not found: ${bookDir}`);
  }

  for (const source of walkFiles(bookDir)) {
    if (!assetExtensions.has(path.extname(source).toLowerCase())) continue;

    const relativePath = path.relative(bookDir, source);
    copyIfExists(source, path.join(distDir, relativePath));
  }
}

export function compileDotFiles() {
  if (!fs.existsSync(bookDir)) return;

  if (!hasCommand('dot', ['-V'])) {
    console.warn('[runtime] dot command not found; skipping .dot svg generation');
    return;
  }

  for (const source of walkFiles(bookDir)) {
    if (path.extname(source).toLowerCase() !== '.dot') continue;

    const relativePath = path.relative(bookDir, source).replace(/\.dot$/i, '.svg');
    const outputPath = path.join(distDir, relativePath);
    fs.mkdirSync(path.dirname(outputPath), { recursive: true });

    runCommand('dot', ['-Tsvg', source, '-o', outputPath], {
      label: `dot build for ${source}`
    });
  }
}

export function compileMarkdownCss(force = true) {
  const scssPath = path.join(appDir, 'markdown-style/markdown.scss');
  if (!fs.existsSync(scssPath)) return false;

  const cssPath = path.join(distDir, 'markdown.css');
  if (!force && fs.existsSync(cssPath)) {
    const cssMtime = fs.statSync(cssPath).mtimeMs;
    const styleFiles = walkFiles(path.join(appDir, 'markdown-style'));
    let newestStyleMtime = 0;
    for (const source of styleFiles) {
      newestStyleMtime = Math.max(newestStyleMtime, fs.statSync(source).mtimeMs);
    }
    if (newestStyleMtime <= cssMtime) return false;
  }

  fs.mkdirSync(distDir, { recursive: true });
  runCommand('npx', [
    'sass',
    '--load-path=packages/rbook-markdown/src/markdown-it/assets',
    scssPath,
    cssPath
  ], {
    cwd: path.resolve(appDir, '..'),
    label: 'markdown css build'
  });
  return true;
}

export function copyStaticAssets() {
  copyIfExists(publicDir, distDir);
  copyIfExists(path.join(appDir, 'theme/assets'), path.join(distDir, 'assets'));
  copyIfExists(
    path.resolve(appDir, '../node_modules/d3/dist/d3.min.js'),
    path.join(distDir, 'assets/vendor/d3.min.js')
  );
  copyIfExists(
    path.join(appDir, 'widgets/animate_single_html'),
    path.join(distDir, 'animate_single_html')
  );
}

export function buildWidgetApp(source: string, target: string) {
  const sourcePath = path.join(appDir, `widgets/${source}/index.html`);
  if (!fs.existsSync(sourcePath)) return;

  const targetPath = path.join(distDir, target, 'index.html');
  fs.mkdirSync(path.dirname(targetPath), { recursive: true });
  fs.copyFileSync(sourcePath, targetPath);
  console.log(`[runtime] copied widget ${source} to ${targetPath}`);
}

export function buildStaticWidgetApps() {
  for (const widget of staticWidgetApps) {
    buildWidgetApp(widget.source, widget.target);
  }
}

export function buildCodeTemplateApp() {
  buildWidgetApp('code_template_filter', 'code_template');
}

export function buildRuntime() {
  assertPreCheck();

  console.log(`[runtime] appDir=${appDir}`);
  console.log(`[runtime] bookDir=${bookDir}`);
  console.log(`[runtime] distDir=${distDir}`);

  resetRuntimeDir();
  const book = new rbook();
  book.build();
  book.build_glob();

  compileMarkdownCss();
  copyStaticAssets();
  copyBookAssets();
  compileDotFiles();
  buildStaticWidgetApps();

  const index = buildSearchIndex();
  console.log(`[runtime] search pages=${index.stats.pages}, codes=${index.stats.codes}, errors=${index.stats.errors}`);
  if (index.stats.errors > 0) {
    throw new Error(`search index has ${index.stats.errors} page errors`);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  try {
    buildRuntime();
  } catch (error) {
    console.error(error instanceof Error ? error.stack || error.message : error);
    process.exit(1);
  }
}
