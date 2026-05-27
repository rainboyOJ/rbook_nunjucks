import { spawnSync } from 'child_process';

export class GumError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'GumError';
  }
}

type GumKwargs = Record<string, string | number | boolean | Array<string | number>>;

function convertKwargsToFlags(kwargs: GumKwargs) {
  const flags: string[] = [];
  for (const [key, value] of Object.entries(kwargs)) {
    const flag = `--${key.replace(/_/g, '-')}`;
    if (typeof value === 'boolean') {
      if (value) {
        flags.push(flag);
      }
    } else if (Array.isArray(value)) {
      for (const item of value) {
        flags.push(flag, String(item));
      }
    } else {
      flags.push(flag, String(value));
    }
  }
  return flags;
}

function runGum(subcommand: string, args: string[] = [], kwargs: GumKwargs = {}) {
  try {
    const command = ['gum', subcommand];
    const flags = convertKwargsToFlags(kwargs);
    command.push(...flags);
    command.push(...args);

    // Using spawnSync to run the command synchronously
    const process = spawnSync(command[0], command.slice(1), {
      stdio: ['inherit', 'pipe', 'inherit'],
      encoding: 'utf-8'
    });

    if (process.status !== 0) {
      // `gum confirm` returning 1 means "No", which isn't a script-crashing error.
      // We let the `confirm` wrapper handle this specific case.
      // For other commands, a non-zero exit code is an error.
      if (!(subcommand === 'confirm' && process.status === 1)) {
        // An empty stderr often means the user cancelled (e.g., pressed Esc).
        if (process.stderr) {
          throw new GumError(`gum ${subcommand} exited with code ${process.status}: ${process.stderr.trim()}`);
        }
        // If no stderr, it's likely a cancellation, so we return an empty string.
        return '';
      }
    }

    if (subcommand === 'confirm') {
      // For confirm, we return true if the return code is 0, false otherwise.
      return process.status === 0;
    }

    return process.stdout ? process.stdout.trim() : '';
  } catch (error) {
    if (error && typeof error === 'object' && 'code' in error && error.code === 'ENOENT') {
      throw new GumError("gum command not found. Is it installed and in your PATH? See: https://github.com/charmbracelet/gum");
    }
    throw new GumError(error instanceof Error ? error.message : String(error));
  }
}

// --- Wrapper Functions ---

export function input(kwargs: GumKwargs = {}) {
  return runGum('input', [], kwargs);
}

export function write(kwargs: GumKwargs = {}) {
  return runGum('write', [], kwargs);
}

export function choose(options: string[], kwargs: GumKwargs = {}) {
  return runGum('choose', options, kwargs);
}

export function filter(options: string[], kwargs: GumKwargs = {}) {
  return runGum('filter', options, kwargs);
}

export function confirm(prompt = "Are you sure?", kwargs: GumKwargs = {}) {
  try {
    return runGum('confirm', [prompt], kwargs);
  } catch (error) {
    // A non-zero return code from confirm will raise a GumError.
    // We catch it and interpret it as "No".
    return false;
  }
}

export function spin(command: string[], title = "Running...", kwargs: GumKwargs = {}) {
  kwargs.title = title;
  return runGum('spin', ['--', ...command], kwargs);
}

export function style(text: string, kwargs: GumKwargs = {}) {
  return runGum('style', [text], kwargs);
}

export function join(parts: string[], kwargs: GumKwargs = {}) {
  return runGum('join', parts, kwargs);
}
