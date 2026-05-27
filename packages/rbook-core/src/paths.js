import path from 'path';
import { fileURLToPath } from 'url';

export const __filename = fileURLToPath(import.meta.url);
export const __dirname = path.dirname(__filename);

export const rootDir = path.resolve(__dirname, '../../..');
export const workdir = rootDir;
export const bookDir = path.join(rootDir, 'book');
export const codeTemplateDir = path.join(rootDir, 'code');
export const themeDir = path.join(rootDir, 'theme');
export const publicDir = path.join(rootDir, 'public');
export const distDir = path.join(rootDir, 'dist');
export const searchDir = path.join(rootDir, '.search');
export const searchIndexPath = path.join(searchDir, 'index.json');
export const configPath = path.join(rootDir, 'book.yaml');

export function fromRoot(...parts) {
  return path.join(rootDir, ...parts);
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
