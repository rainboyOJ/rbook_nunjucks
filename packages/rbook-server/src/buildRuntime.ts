import fs from 'fs';
import path from 'path';
import { spawnSync } from 'child_process';
import rbook from '@rbook/core';
import {
  appDir,
  bookDir,
  distDir,
  publicDir,
  runtimeDir
} from '@rbook/core/paths';
import { buildSearchIndex } from '@rbook/search/buildIndex';

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

function resetRuntimeDir() {
  if (!runtimeDir) {
    fs.rmSync(distDir, { recursive: true, force: true });
    return;
  }

  fs.rmSync(runtimeDir, { recursive: true, force: true });
  fs.mkdirSync(runtimeDir, { recursive: true });
}

function copyIfExists(src: string, dest: string) {
  if (!fs.existsSync(src)) return;
  fs.mkdirSync(path.dirname(dest), { recursive: true });
  fs.cpSync(src, dest, { recursive: true });
}

function copyBookAssets() {
  if (!fs.existsSync(bookDir)) {
    throw new Error(`book directory not found: ${bookDir}`);
  }

  const walk = (dir: string) => {
    for (const entry of fs.readdirSync(dir)) {
      const source = path.join(dir, entry);
      const stat = fs.statSync(source);
      if (stat.isDirectory()) {
        walk(source);
        continue;
      }

      if (!stat.isFile()) continue;
      if (!assetExtensions.has(path.extname(entry).toLowerCase())) continue;

      const relativePath = path.relative(bookDir, source);
      copyIfExists(source, path.join(distDir, relativePath));
    }
  };

  walk(bookDir);
}

function compileDotFiles() {
  if (!fs.existsSync(bookDir)) return;

  const dotVersion = spawnSync('dot', ['-V'], {
    stdio: 'ignore'
  });
  if (dotVersion.error) {
    console.warn('[runtime] dot command not found; skipping .dot svg generation');
    return;
  }

  const walk = (dir: string) => {
    for (const entry of fs.readdirSync(dir)) {
      const source = path.join(dir, entry);
      const stat = fs.statSync(source);
      if (stat.isDirectory()) {
        walk(source);
        continue;
      }

      if (!stat.isFile() || path.extname(entry).toLowerCase() !== '.dot') {
        continue;
      }

      const relativePath = path.relative(bookDir, source).replace(/\.dot$/i, '.svg');
      const outputPath = path.join(distDir, relativePath);
      fs.mkdirSync(path.dirname(outputPath), { recursive: true });

      const result = spawnSync('dot', ['-Tsvg', source, '-o', outputPath], {
        stdio: 'inherit'
      });

      if (result.error) {
        throw result.error;
      }

      if (result.status !== 0) {
        throw new Error(`dot build failed for ${source} with status ${result.status}`);
      }
    }
  };

  walk(bookDir);
}

function compileMarkdownCss() {
  const scssPath = path.join(appDir, 'markdown-style/markdown.scss');
  if (!fs.existsSync(scssPath)) return;

  fs.mkdirSync(distDir, { recursive: true });
  const result = spawnSync('npx', [
    'sass',
    '--load-path=packages/rbook-markdown/src/markdown-it/assets',
    scssPath,
    path.join(distDir, 'markdown.css')
  ], {
    cwd: path.resolve(appDir, '..'),
    stdio: 'inherit'
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    throw new Error(`markdown css build failed with status ${result.status}`);
  }
}

function copyStaticAssets() {
  copyIfExists(publicDir, distDir);
  copyIfExists(path.join(appDir, 'theme/assets'), path.join(distDir, 'assets'));
  copyIfExists(
    path.join(appDir, 'widgets/animate_single_html'),
    path.join(distDir, 'animate_single_html')
  );
}

function buildCodeTemplateApp() {
  const configPath = path.join(appDir, 'widgets/code_template_filter/vite.config.ts');
  if (!fs.existsSync(configPath)) return;

  const result = spawnSync('npx', [
    'vite',
    'build',
    '--config',
    configPath,
    '--base',
    '/code_template/'
  ], {
    cwd: path.resolve(appDir, '..'),
    stdio: 'inherit'
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    throw new Error(`code template build failed with status ${result.status}`);
  }
}

export function buildRuntime() {
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
  buildCodeTemplateApp();

  const index = buildSearchIndex();
  console.log(`[runtime] search pages=${index.stats.pages}, chunks=${index.stats.chunks}, errors=${index.stats.errors}`);
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
