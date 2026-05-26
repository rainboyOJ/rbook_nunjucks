import path from 'path';
import { fileURLToPath } from 'url';

export const __filename = fileURLToPath(import.meta.url);
export const __dirname = path.dirname(__filename);
export const workdir = path.resolve(__dirname, '../..');
export const bookDir = path.join(workdir, 'book');
export const distDir = path.join(workdir, 'dist');
export const searchDir = path.join(workdir, '.search');
export const searchIndexPath = path.join(searchDir, 'index.json');

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
