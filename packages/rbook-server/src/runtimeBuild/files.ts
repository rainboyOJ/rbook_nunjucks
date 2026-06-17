import fs from 'fs';
import path from 'path';

// 运行时构建只需要很小的文件工具：存在就复制，以及递归枚举文件。
// 放在独立模块后，buildRuntime.ts 可以只保留“构建顺序”。
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
