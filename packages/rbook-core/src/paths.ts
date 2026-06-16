import path from 'path';
import { fileURLToPath } from 'url';

export const __filename = fileURLToPath(import.meta.url);
export const __dirname = path.dirname(__filename);

export const rootDir = path.resolve(__dirname, '../../..');
export const workdir = rootDir;
export const defaultAppDir = path.join(rootDir, 'apps/algorithm-book');
export const defaultContentDir = path.join(rootDir, 'content/algorithm-book');
export const appDir = process.env.RBOOK_APP_DIR
  ? path.resolve(rootDir, process.env.RBOOK_APP_DIR)
  : defaultAppDir;
export const contentDir = process.env.RBOOK_CONTENT_DIR
  ? path.resolve(process.env.RBOOK_CONTENT_DIR)
  : defaultContentDir;
export const runtimeDir = process.env.RBOOK_RUNTIME_DIR
  ? path.resolve(process.env.RBOOK_RUNTIME_DIR)
  : '';
export const bookDir = contentDir
  ? path.join(contentDir, 'book')
  : path.join(appDir, 'book');
export const codeTemplateDir = process.env.RBOOK_CODE_DIR
  ? path.resolve(process.env.RBOOK_CODE_DIR)
  : path.join(contentDir, 'code');
export const themeDir = path.join(appDir, 'theme');
export const publicDir = path.join(appDir, 'public');
export const distDir = runtimeDir
  ? path.join(runtimeDir, 'dist')
  : path.join(appDir, 'dist');
export const searchDir = runtimeDir
  ? path.join(runtimeDir, '.search')
  : path.join(appDir, '.search');
export const searchIndexPath = path.join(searchDir, 'index.json');
export const configPath = contentDir
  ? path.join(contentDir, 'book.yaml')
  : path.join(appDir, 'book.yaml');

export function fromRoot(...parts) {
  return path.join(rootDir, ...parts);
}

export function fromApp(...parts) {
  return path.join(appDir, ...parts);
}

export function toPosixPath(filePath) {
  return filePath.split(path.sep).join('/');
}

export function toBookPath(filePath) {
  return toPosixPath(path.relative(bookDir, filePath));
}

export function bookPathToUrl(bookPath) {
  const normalized = toPosixPath(bookPath);
  return `/${normalized.replace(/\.md$/, '.html')}`;
}
