import { spawnSync } from 'child_process';
import type { StdioOptions } from 'child_process';

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
