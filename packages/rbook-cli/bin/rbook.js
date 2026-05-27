#!/usr/bin/env node

import chalk from 'chalk';
import { run } from '../src/index.js';

try {
  run();
} catch (error) {
  console.error(chalk.red('构建失败:'), chalk.blue(error.message));
  process.exit(1);
}
