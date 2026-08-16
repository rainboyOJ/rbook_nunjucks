#!/usr/bin/env node

import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const [input, requestedOutputDir] = process.argv.slice(2);

if (!input) {
  console.error('Usage: node render-svg.mjs <file.svg> [output-directory]');
  process.exit(2);
}

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const source = path.resolve(input);
const baseName = path.basename(source, '.svg');
const outputDir = requestedOutputDir
  ? path.resolve(requestedOutputDir)
  : path.join(os.tmpdir(), 'rbook-svg-preview', baseName);

const validation = spawnSync(process.execPath, [path.join(scriptDir, 'validate-svg.mjs'), source], {
  encoding: 'utf8',
  stdio: 'inherit'
});
if (validation.status !== 0) process.exit(validation.status ?? 1);

fs.mkdirSync(outputDir, { recursive: true });

function render(width, name) {
  const output = path.join(outputDir, name);
  const result = spawnSync('rsvg-convert', ['-f', 'png', '-w', String(width), '-o', output, source], {
    encoding: 'utf8'
  });
  if (result.error?.code === 'ENOENT') {
    console.error('rsvg-convert is required but was not found');
    process.exit(1);
  }
  if (result.status !== 0) {
    console.error((result.stderr || result.stdout || 'rsvg-convert failed').trim());
    process.exit(result.status ?? 1);
  }
  return output;
}

const desktop = render(720, 'desktop-720.png');
const mobile = render(360, 'mobile-360.png');

console.log(`desktop: ${desktop}`);
console.log(`mobile:  ${mobile}`);
