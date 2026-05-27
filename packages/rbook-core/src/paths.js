import path from 'path';
import { fileURLToPath } from 'url';

export const __filename = fileURLToPath(import.meta.url);
export const __dirname = path.dirname(__filename);

export const rootDir = path.resolve(__dirname, '../../..');
export const workdir = rootDir;
export const defaultAppDir = path.join(rootDir, 'apps/algorithm-book');
export const appDir = process.env.RBOOK_APP_DIR
  ? path.resolve(rootDir, process.env.RBOOK_APP_DIR)
  : defaultAppDir;
export const bookDir = path.join(appDir, 'book');
export const codeTemplateDir = path.join(rootDir, 'code');
export const themeDir = path.join(appDir, 'theme');
export const publicDir = path.join(appDir, 'public');
export const distDir = path.join(appDir, 'dist');
export const searchDir = path.join(appDir, '.search');
export const searchIndexPath = path.join(searchDir, 'index.json');
export const configPath = path.join(appDir, 'book.yaml');

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
