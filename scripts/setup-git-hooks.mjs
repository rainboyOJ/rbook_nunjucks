import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const expectedHooksPath = '.githooks';

function runGit(args, options = {}) {
  return execFileSync('git', args, {
    cwd: scriptDir,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
    ...options
  }).trim();
}

let repositoryRoot;
try {
  repositoryRoot = runGit(['rev-parse', '--show-toplevel']);
} catch {
  console.error('[setup:hooks] 当前目录不在 Git 仓库中。');
  process.exit(1);
}

const hooksDirectory = path.join(repositoryRoot, expectedHooksPath);
const prePushPath = path.join(hooksDirectory, 'pre-push');
if (!fs.existsSync(prePushPath)) {
  console.error(`[setup:hooks] 找不到 hook 文件：${prePushPath}`);
  process.exit(1);
}

let configuredPath = '';
try {
  configuredPath = runGit(['config', '--local', '--get', 'core.hooksPath']);
} catch {
  configuredPath = '';
}

if (configuredPath && configuredPath !== expectedHooksPath) {
  console.error(`[setup:hooks] 已存在其他 core.hooksPath：${configuredPath}`);
  console.error(`[setup:hooks] 为避免覆盖现有 hooks，未修改 Git 配置。需要时请手动切换到 ${expectedHooksPath}。`);
  process.exit(1);
}

if (configuredPath === expectedHooksPath) {
  console.log(`[setup:hooks] hooks 已启用：${expectedHooksPath}`);
  process.exit(0);
}

runGit(['config', '--local', 'core.hooksPath', expectedHooksPath]);
console.log(`[setup:hooks] 已启用版本化 hooks：${expectedHooksPath}`);
