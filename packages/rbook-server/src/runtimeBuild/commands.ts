import { spawnSync } from 'child_process';
import type { StdioOptions } from 'child_process';

// 运行时构建会调用 sass、dot、vite 等外部命令。
// 这里统一处理退出码和错误信息，避免 buildRuntime.ts 里重复写 spawnSync 样板代码。
interface RunCommandOptions {
  cwd?: string;
  label: string;
  stdio?: StdioOptions;
}

export function hasCommand(command: string, args: string[] = ['--version']) {
  const result = spawnSync(command, args, {
    stdio: 'ignore'
  });

  return !result.error && result.status === 0;
}

export function runCommand(command: string, args: string[], options: RunCommandOptions) {
  const result = spawnSync(command, args, {
    cwd: options.cwd,
    stdio: options.stdio ?? 'inherit'
  });

  if (result.error) {
    throw result.error;
  }

  if (result.status !== 0) {
    throw new Error(`${options.label} failed with status ${result.status}`);
  }
}
