#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import yaml from 'js-yaml';

const DEFAULT_PROBLEMS_DIR =
  process.env.RBOOK_PROBLEM_SOLUTIONS_DIR ||
  '/home/rainboy/mycode/rbook_new_problem_solutions/problems';
const WEBSITE_ORIGIN = 'https://pcs2.roj.ac.cn';
const IGNORED_DIRECTORIES = new Set(['.git', 'node_modules', 'problem-analysis-workspace', 'duipai-failed']);

function fail(message) {
  console.error(message);
  process.exit(2);
}

function parseArgs(argv) {
  const [command, ...rest] = argv;
  const options = {};

  for (let index = 0; index < rest.length; index += 1) {
    const argument = rest[index];
    if (!argument.startsWith('--')) fail(`Unexpected argument: ${argument}`);

    const key = argument.slice(2);
    const value = rest[index + 1];
    if (!value || value.startsWith('--')) fail(`Missing value for --${key}`);
    options[key] = value;
    index += 1;
  }

  return { command, options };
}

function readFrontMatter(filePath) {
  const raw = fs.readFileSync(filePath, 'utf8');
  const match = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n?/);
  if (!match) return null;

  const data = yaml.load(match[1]);
  if (!data || typeof data !== 'object' || Array.isArray(data)) return null;
  return { data, body: raw.slice(match[0].length) };
}

function walkIndexFiles(directory, files = []) {
  for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
    if (entry.isDirectory()) {
      if (!IGNORED_DIRECTORIES.has(entry.name)) {
        walkIndexFiles(path.join(directory, entry.name), files);
      }
      continue;
    }

    if (entry.isFile() && entry.name === 'index.md') {
      files.push(path.join(directory, entry.name));
    }
  }
  return files;
}

function text(value) {
  return typeof value === 'string' ? value.trim() : '';
}

function catalog(root) {
  if (!fs.existsSync(root) || !fs.statSync(root).isDirectory()) {
    fail(`Problems directory does not exist: ${root}`);
  }

  return walkIndexFiles(root).flatMap((filePath) => {
    try {
      const parsed = readFrontMatter(filePath);
      const oj = text(parsed?.data.oj);
      const problemId = text(parsed?.data.problem_id);
      if (!oj || !problemId) return [];

      const relativePath = path.relative(root, filePath);
      return [{
        oj,
        problem_id: problemId,
        title: text(parsed.data.title),
        description: text(parsed.data.description),
        difficulty: text(parsed.data.difficulty),
        tags: Array.isArray(parsed.data.tags) ? parsed.data.tags.filter((tag) => typeof tag === 'string') : [],
        md_path: relativePath,
        url: `/problems/${oj}/${problemId}`,
        solution_url: `${WEBSITE_ORIGIN}/problems/${oj}/${problemId}`,
        searchable_text: `${parsed.data.title || ''}\n${parsed.data.description || ''}\n${(parsed.data.tags || []).join(' ')}\n${parsed.body}`.toLowerCase()
      }];
    } catch {
      return [];
    }
  });
}

function publicProblem(problem) {
  const { searchable_text, ...publicProblem } = problem;
  return publicProblem;
}

function canonicalId(oj, id) {
  const trimmed = id.trim();
  if (oj.toLowerCase() === 'luogu' && /^p?\d+$/i.test(trimmed)) {
    return `P${trimmed.replace(/^p/i, '')}`;
  }
  return trimmed;
}

function lookup(items, requestedOj, requestedId) {
  const oj = requestedOj.trim();
  const matchingOjs = items.filter((problem) => problem.oj.toLowerCase() === oj.toLowerCase());
  const id = canonicalId(matchingOjs[0]?.oj || oj, requestedId);
  const problem = matchingOjs.find((item) => item.problem_id.toLowerCase() === id.toLowerCase());

  return {
    query: { oj, problem_id: requestedId.trim() },
    canonical_query: { oj: matchingOjs[0]?.oj || oj, problem_id: id },
    found: Boolean(problem),
    problem: problem ? publicProblem(problem) : null
  };
}

function queryTerms(query) {
  return [...new Set((query.toLowerCase().match(/[a-z0-9_+-]+|[\u4e00-\u9fff]+/g) || []).filter(Boolean))];
}

function search(items, query, limit) {
  const terms = queryTerms(query);
  const matches = items.map((problem) => {
    let score = 0;
    const tags = problem.tags.join(' ').toLowerCase();
    const title = problem.title.toLowerCase();
    const description = problem.description.toLowerCase();

    for (const term of terms) {
      if (tags.includes(term)) score += 100;
      if (title.includes(term)) score += 40;
      if (description.includes(term)) score += 20;
      if (problem.searchable_text.includes(term)) score += 5;
    }

    return { problem, score };
  }).filter((item) => item.score > 0);

  matches.sort((a, b) => b.score - a.score || a.problem.oj.localeCompare(b.problem.oj) || a.problem.problem_id.localeCompare(b.problem.problem_id));

  return {
    query,
    terms,
    count: Math.min(limit, matches.length),
    candidates: matches.slice(0, limit).map(({ problem, score }) => ({ score, ...publicProblem(problem) }))
  };
}

const { command, options } = parseArgs(process.argv.slice(2));
const root = path.resolve(options.root || DEFAULT_PROBLEMS_DIR);
const items = catalog(root);

if (command === 'lookup') {
  if (!options.oj || !options.id) fail('Usage: problem-catalog.mjs lookup --oj <oj> --id <problem_id> [--root <problems_dir>]');
  console.log(JSON.stringify(lookup(items, options.oj, options.id), null, 2));
} else if (command === 'search') {
  if (!options.query) fail('Usage: problem-catalog.mjs search --query <keywords> [--limit <count>] [--root <problems_dir>]');
  const limit = Number.parseInt(options.limit || '5', 10);
  if (!Number.isInteger(limit) || limit < 1) fail('--limit must be a positive integer');
  console.log(JSON.stringify(search(items, options.query, limit), null, 2));
} else {
  fail('Usage: problem-catalog.mjs <lookup|search> ...');
}
