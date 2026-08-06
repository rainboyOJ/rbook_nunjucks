import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { isDeepStrictEqual } from 'node:util';
import matter from 'gray-matter';
import yaml from 'js-yaml';

const malformedMarker = /^---id:/m;
const allowedSyntheticKeys = new Set(['id', 'title']);

// These five marker IDs were early candidates that collide with existing pages.
// The synthetic block contains the later, globally unique ID selected by migration.
const resolvedIds = new Map([
  ['data_structure/RBTree/index.md', 'data-structure-rbtree-1'],
  ['graph/网络流/最大权闭合子图/index.md', 'graph-7'],
  ['graph/网络流/最小点覆盖与最大点权独立集/index.md', 'graph-10'],
  ['graph/网络流/最小路径覆盖/index.md', 'graph-11'],
  ['graph/网络流/最小路径覆盖/最小路径覆盖.md', 'graph-12']
]);

function fail(filePath, message) {
  throw new Error(`${filePath}: ${message}`);
}

function parseIdToken(token, filePath, label) {
  let value;
  try {
    value = yaml.load(`id: ${token}\n`)?.id;
  } catch (error) {
    fail(filePath, `${label} ID is invalid YAML: ${error instanceof Error ? error.message : String(error)}`);
  }

  if (typeof value !== 'string' || value.length === 0) {
    fail(filePath, `${label} ID must be a non-empty string`);
  }
  return value;
}

function parseYamlMapping(source, filePath) {
  let value;
  try {
    value = yaml.load(source) || {};
  } catch (error) {
    fail(filePath, `original front matter is invalid YAML: ${error instanceof Error ? error.message : String(error)}`);
  }

  if (typeof value !== 'object' || Array.isArray(value)) {
    fail(filePath, 'original front matter must be a YAML mapping');
  }
  return value;
}

export function repairMalformedFrontMatter(source, options = {}) {
  const filePath = options.filePath || '<input>';
  if (!malformedMarker.test(source)) {
    return { changed: false, content: source, id: null, resolvedConflict: false };
  }

  const syntheticBlock = /^---(\r?\n)([\s\S]*?)\r?\n---(\r?\n|$)/.exec(source);
  if (!syntheticBlock) {
    fail(filePath, 'cannot parse the synthetic front matter block');
  }

  const parsedSynthetic = matter(source).data;
  const unexpectedKeys = Object.keys(parsedSynthetic).filter((key) => !allowedSyntheticKeys.has(key));
  if (unexpectedKeys.length > 0) {
    fail(filePath, `synthetic block has unexpected keys: ${unexpectedKeys.join(', ')}`);
  }
  if (typeof parsedSynthetic.id !== 'string' || parsedSynthetic.id.length === 0) {
    fail(filePath, 'synthetic ID must be a non-empty string');
  }

  const idLines = [...syntheticBlock[2].matchAll(/^id:[ \t]*(.+?)[ \t]*$/gm)];
  if (idLines.length !== 1) {
    fail(filePath, `synthetic block must contain exactly one ID line, found ${idLines.length}`);
  }
  const syntheticId = parseIdToken(idLines[0][1], filePath, 'synthetic');
  if (syntheticId !== parsedSynthetic.id) {
    fail(filePath, 'synthetic ID token does not match parsed front matter');
  }

  const afterSynthetic = source.slice(syntheticBlock[0].length);
  const damagedStart = /^(\r?\n*)---id:[ \t]*([^\r\n]+)(\r?\n)/.exec(afterSynthetic);
  if (!damagedStart) {
    fail(filePath, 'damaged original block is not immediately after the synthetic block');
  }

  const markerId = parseIdToken(damagedStart[2], filePath, 'damaged marker');
  const resolvedConflict = markerId !== syntheticId;
  if (resolvedConflict && options.expectedId !== syntheticId) {
    fail(
      filePath,
      `ID conflict: synthetic='${syntheticId}', marker='${markerId}'; an explicit expectedId is required`
    );
  }
  if (options.expectedId !== undefined && options.expectedId !== syntheticId) {
    fail(filePath, `expectedId '${options.expectedId}' does not match synthetic ID '${syntheticId}'`);
  }

  const afterDamagedStart = afterSynthetic.slice(damagedStart[0].length);
  const closing = /^---[ \t]*(\r?\n|$)/m.exec(afterDamagedStart);
  if (!closing) {
    fail(filePath, 'original front matter has no closing delimiter');
  }

  const originalYaml = afterDamagedStart
    .slice(0, closing.index)
    .replace(/^(?:\r?\n)+/, '')
    .replace(/(?:\r?\n)+$/, '');
  const originalData = parseYamlMapping(originalYaml, filePath);
  if (Object.prototype.hasOwnProperty.call(originalData, 'id')) {
    fail(filePath, 'original front matter unexpectedly already contains an ID');
  }

  const newline = syntheticBlock[1];
  const idLine = `id: ${idLines[0][1].trim()}`;
  const mergedYaml = originalYaml ? `${idLine}${newline}${originalYaml}` : idLine;
  const body = afterDamagedStart.slice(closing.index + closing[0].length);
  const repaired = `---${newline}${mergedYaml}${newline}---${closing[1]}${body}`;
  const parsedRepaired = matter(repaired);

  if (parsedRepaired.data.id !== syntheticId) {
    fail(filePath, 'repaired front matter does not contain the expected ID');
  }
  for (const [key, value] of Object.entries(originalData)) {
    if (!isDeepStrictEqual(parsedRepaired.data[key], value)) {
      fail(filePath, `repaired front matter changed field '${key}'`);
    }
  }
  if (parsedRepaired.content !== body) {
    fail(filePath, 'repair changed the Markdown body');
  }
  if (malformedMarker.test(repaired)) {
    fail(filePath, 'repair left a malformed ---id marker behind');
  }

  return {
    changed: true,
    content: repaired,
    id: syntheticId,
    markerId,
    resolvedConflict,
    originalData,
    body
  };
}

function collectMarkdownFiles(root) {
  const result = [];
  const visit = (directory) => {
    for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
      const fullPath = path.join(directory, entry.name);
      if (entry.isDirectory()) visit(fullPath);
      else if (entry.isFile() && entry.name.endsWith('.md')) result.push(fullPath);
    }
  };
  visit(root);
  return result.sort((left, right) => left.localeCompare(right, 'zh-CN'));
}

function validateAllPageIds(files, proposedContent, pagesRoot) {
  const seen = new Map();
  for (const filePath of files) {
    const relativePath = path.relative(pagesRoot, filePath).split(path.sep).join('/');
    const source = proposedContent.get(filePath) || fs.readFileSync(filePath, 'utf8');
    const id = matter(source).data.id;
    if (typeof id !== 'string' || id.length === 0) {
      fail(relativePath, 'page ID must be a non-empty string after repair');
    }
    const oldPath = seen.get(id);
    if (oldPath) {
      fail(relativePath, `duplicate page ID '${id}' also used by ${oldPath}`);
    }
    seen.set(id, relativePath);
  }
  return seen.size;
}

function runCli() {
  const args = process.argv.slice(2);
  const unknownArgs = args.filter((arg) => arg !== '--write');
  if (unknownArgs.length > 0) {
    console.error(`Unknown arguments: ${unknownArgs.join(', ')}`);
    process.exitCode = 2;
    return;
  }

  const write = args.includes('--write');
  const pagesRoot = path.resolve('book/pages');
  const files = collectMarkdownFiles(pagesRoot);
  const proposedContent = new Map();
  const reports = [];
  const errors = [];
  let pageCount = 0;

  for (const filePath of files) {
    const source = fs.readFileSync(filePath, 'utf8');
    if (!malformedMarker.test(source)) continue;

    const relativePath = path.relative(pagesRoot, filePath).split(path.sep).join('/');
    try {
      const result = repairMalformedFrontMatter(source, {
        filePath: relativePath,
        expectedId: resolvedIds.get(relativePath)
      });
      proposedContent.set(filePath, result.content);
      reports.push({ relativePath, ...result });
    } catch (error) {
      errors.push(error instanceof Error ? error.message : String(error));
    }
  }

  if (errors.length === 0) {
    try {
      pageCount = validateAllPageIds(files, proposedContent, pagesRoot);
    } catch (error) {
      errors.push(error instanceof Error ? error.message : String(error));
    }
  }

  if (errors.length > 0) {
    for (const error of errors) console.error(`[frontmatter-fix] ERROR ${error}`);
    console.error(`[frontmatter-fix] aborted with ${errors.length} error(s); no files were written`);
    process.exitCode = 1;
    return;
  }

  for (const report of reports) {
    const resolution = report.resolvedConflict ? ` (resolved marker ID '${report.markerId}')` : '';
    console.log(`[frontmatter-fix] ${report.relativePath}: id=${report.id}${resolution}`);
  }

  if (write) {
    for (const [filePath, content] of proposedContent) {
      fs.writeFileSync(filePath, content, 'utf8');
    }
  }

  console.log(
    `[frontmatter-fix] mode=${write ? 'write' : 'dry-run'} pages=${pageCount} files=${reports.length} conflicts=${reports.filter((item) => item.resolvedConflict).length}`
  );
}

const isDirectRun = process.argv[1]
  && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isDirectRun) runCli();
