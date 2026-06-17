import fs from 'fs';
import path from 'path';

export function copyIfExists(src: string, dest: string) {
  if (!fs.existsSync(src)) return;
  fs.mkdirSync(path.dirname(dest), { recursive: true });
  fs.cpSync(src, dest, { recursive: true });
}

export function* walkFiles(dir: string): Generator<string> {
  if (!fs.existsSync(dir)) return;

  for (const entry of fs.readdirSync(dir)) {
    const source = path.join(dir, entry);
    const stat = fs.statSync(source);

    if (stat.isDirectory()) {
      yield* walkFiles(source);
      continue;
    }

    if (stat.isFile()) {
      yield source;
    }
  }
}
