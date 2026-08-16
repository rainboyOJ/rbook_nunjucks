#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

const files = process.argv.slice(2);

if (files.length === 0) {
  console.error('Usage: node validate-svg.mjs <file.svg> [more.svg ...]');
  process.exit(2);
}

function runXmllint(args, file) {
  const result = spawnSync('xmllint', ['--nonet', ...args, file], {
    encoding: 'utf8'
  });
  if (result.error?.code === 'ENOENT') {
    throw new Error('xmllint is required but was not found');
  }
  if (result.status !== 0) {
    throw new Error((result.stderr || result.stdout || 'xmllint failed').trim());
  }
  return result.stdout.trim();
}

function xpath(file, expression) {
  return runXmllint(['--xpath', expression], file);
}

function count(file, expression) {
  return Number(xpath(file, `count(${expression})`));
}

function attr(file, name) {
  return xpath(file, `string(/*[local-name()='svg']/@${name})`);
}

function numericDimension(value) {
  return /^\d+(?:\.\d+)?$/.test(value) ? Number(value) : null;
}

let hasErrors = false;

for (const input of files) {
  const file = path.resolve(input);
  const errors = [];
  const warnings = [];

  if (!fs.existsSync(file)) {
    console.error(`FAIL ${input}\n  - file does not exist`);
    hasErrors = true;
    continue;
  }
  if (!fs.statSync(file).isFile()) {
    console.error(`FAIL ${input}\n  - path is not a file`);
    hasErrors = true;
    continue;
  }
  if (!/^[a-z0-9]+(?:-[a-z0-9]+)*\.svg$/.test(path.basename(file))) {
    errors.push('filename must use descriptive kebab-case');
  }

  const source = fs.readFileSync(file, 'utf8');
  if (/<!DOCTYPE|<!ENTITY/i.test(source)) {
    errors.push('DOCTYPE and ENTITY declarations are forbidden');
  }

  try {
    runXmllint(['--noout'], file);

    const viewBox = attr(file, 'viewBox');
    const widthRaw = attr(file, 'width');
    const heightRaw = attr(file, 'height');
    const role = attr(file, 'role');
    const labelledBy = attr(file, 'aria-labelledby').split(/\s+/).filter(Boolean);
    const titleId = xpath(file, "string(/*[local-name()='svg']/*[local-name()='title'][1]/@id)");
    const descId = xpath(file, "string(/*[local-name()='svg']/*[local-name()='desc'][1]/@id)");
    const titleText = xpath(file, "normalize-space(string(/*[local-name()='svg']/*[local-name()='title'][1]))");
    const descText = xpath(file, "normalize-space(string(/*[local-name()='svg']/*[local-name()='desc'][1]))");

    const viewBoxParts = viewBox.split(/[\s,]+/).map(Number);
    if (viewBoxParts.length !== 4 || viewBoxParts.some(Number.isNaN)) {
      errors.push('root svg must have a numeric four-part viewBox');
    }

    const width = numericDimension(widthRaw);
    const height = numericDimension(heightRaw);
    if (width === null || height === null) {
      errors.push('width and height must be unitless numeric values');
    } else if (viewBoxParts.length === 4) {
      if (width !== viewBoxParts[2] || height !== viewBoxParts[3]) {
        errors.push('width and height must match the viewBox dimensions');
      }
      if (viewBoxParts[2] < 560 || viewBoxParts[2] > 720) {
        warnings.push('viewBox width is outside the preferred 560-720 range');
      }
    }

    if (role !== 'img') errors.push('root svg must set role="img"');
    if (count(file, "/*[local-name()='svg']/*[local-name()='title']") !== 1 || !titleText) {
      errors.push('root svg must contain exactly one non-empty title');
    }
    if (count(file, "/*[local-name()='svg']/*[local-name()='desc']") !== 1 || !descText) {
      errors.push('root svg must contain exactly one non-empty desc');
    }
    if (!titleId || !descId || !labelledBy.includes(titleId) || !labelledBy.includes(descId)) {
      errors.push('aria-labelledby must reference both title and desc ids');
    }
    if (count(file, "/*[local-name()='svg']/*[local-name()='style']") !== 1) {
      errors.push('root svg must contain exactly one direct style element');
    }
    if (count(file, "//*[local-name()='rect' and contains(concat(' ', normalize-space(@class), ' '), ' canvas ')]") < 1) {
      errors.push('svg must include a rect with class="canvas"');
    }

    const unsafeElements = count(
      file,
      "//*[local-name()='script' or local-name()='foreignObject' or local-name()='image' or local-name()='iframe' or local-name()='audio' or local-name()='video' or local-name()='metadata' or local-name()='a']"
    );
    if (unsafeElements > 0) {
      errors.push('script, foreignObject, image, media, metadata, and links are forbidden');
    }
    if (count(file, "//@*[starts-with(translate(local-name(), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'on')]") > 0) {
      errors.push('event handler attributes are forbidden');
    }
    if (count(file, "//@*[contains(., 'http://') or contains(., 'https://') or starts-with(., 'data:')]") > 0) {
      errors.push('remote and data resources are forbidden');
    }
  } catch (error) {
    errors.push(error.message);
  }

  if (/@import|@font-face|url\(\s*['"]?(?:https?:|data:|\/|\.{1,2}\/)/i.test(source)) {
    errors.push('external CSS, fonts, and resources are forbidden');
  }

  const fontSizes = [...source.matchAll(/font-size\s*:\s*(\d+(?:\.\d+)?)px/gi)]
    .map((match) => Number(match[1]));
  if (fontSizes.some((size) => size < 14)) {
    warnings.push('a CSS font-size is below the 14px design minimum');
  }

  if (errors.length > 0) {
    hasErrors = true;
    console.error(`FAIL ${input}`);
    for (const error of errors) console.error(`  - ${error}`);
  } else {
    console.log(`PASS ${input}`);
  }
  for (const warning of warnings) console.warn(`  warning: ${warning}`);
}

if (hasErrors) process.exit(1);
