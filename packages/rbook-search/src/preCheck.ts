import {
  loadCodeConfig,
  validateCodes,
  validatePages,
  validateReferences,
  type CodeTemplateItem,
  type ValidationError
} from '@rbook/core';
import { collectPages } from './collectPages.js';
import { loadPageDocument } from './markdownText.js';
import type { PageDocument } from './types.js';

export interface PreCheckStats {
  pages: number;
  codes: number;
  errors: number;
  warnings: number;
}

export interface PreCheckResult {
  errors: ValidationError[];
  warnings: ValidationError[];
  stats: PreCheckStats;
  ok: boolean;
}

export interface PreCheckContext {
  result: PreCheckResult;
  pages: PageDocument[];
  codes: CodeTemplateItem[];
  site: Record<string, unknown>;
}

function formatIssue(issue: ValidationError) {
  return `[${issue.level}] ${issue.filePath}: ${issue.message}`;
}

export function evaluatePreCheck(
  pages: PageDocument[],
  codes: CodeTemplateItem[],
  initialIssues: ValidationError[] = []
): PreCheckResult {
  const issues = [
    ...initialIssues,
    ...validateCodes(codes),
    ...validatePages(pages),
    ...validateReferences(pages, codes)
  ];
  const errors = issues.filter((item) => item.level === 'ERROR');
  const warnings = issues.filter((item) => item.level === 'WARNING');

  return {
    errors,
    warnings,
    stats: {
      pages: pages.length,
      codes: codes.length,
      errors: errors.length,
      warnings: warnings.length
    },
    ok: errors.length === 0
  };
}

export function runPreCheckContext(): PreCheckContext {
  const pages: PageDocument[] = [];
  const issues: ValidationError[] = [];
  let site: Record<string, unknown> = {};

  let collected;
  try {
    collected = collectPages();
    site = collected.site as Record<string, unknown>;
  } catch (error) {
    issues.push({
      level: 'ERROR',
      filePath: 'book/book.yaml',
      message: error instanceof Error ? error.message : String(error)
    });
    return {
      result: evaluatePreCheck(pages, [], issues),
      pages,
      codes: [],
      site
    };
  }

  for (const page of collected.pages) {
    try {
      pages.push(loadPageDocument(page));
    } catch (error) {
      issues.push({
        level: 'ERROR',
        filePath: page.path,
        message: `failed to load page: ${error instanceof Error ? error.message : String(error)}`
      });
    }
  }

  let codes: CodeTemplateItem[] = [];
  try {
    codes = loadCodeConfig({ strict: true }).codes;
  } catch (error) {
    issues.push({
      level: 'ERROR',
      filePath: 'book/code.yaml',
      message: error instanceof Error ? error.message : String(error)
    });
  }

  return {
    result: evaluatePreCheck(pages, codes, issues),
    pages,
    codes,
    site
  };
}

export function runPreCheck(): PreCheckResult {
  return runPreCheckContext().result;
}

export function reportPreCheck(result: PreCheckResult) {
  for (const issue of result.errors) console.error(formatIssue(issue));
  for (const issue of result.warnings) console.warn(formatIssue(issue));
  console.log(
    `[pre-check] pages=${result.stats.pages}, codes=${result.stats.codes}, errors=${result.stats.errors}, warnings=${result.stats.warnings}`
  );
}

export function assertPreCheck(): PreCheckResult {
  const result = runPreCheck();
  reportPreCheck(result);
  if (!result.ok) {
    throw new Error(`pre-check failed with ${result.stats.errors} error(s)`);
  }
  return result;
}

export function assertPreCheckContext(): PreCheckContext {
  const context = runPreCheckContext();
  reportPreCheck(context.result);
  if (!context.result.ok) {
    throw new Error(`pre-check failed with ${context.result.stats.errors} error(s)`);
  }
  return context;
}

export function validatePageDocument(
  page: PageDocument,
  allPages: PageDocument[],
  codes: CodeTemplateItem[]
) {
  // Only the changed Markdown file is read again; the other documents are
  // metadata already loaded by the startup pre-check.
  const otherPages = allPages.filter((item) => item.path !== page.path);
  const pageIssues = validatePages([...otherPages, page])
    .filter((issue) => issue.filePath === page.path);
  return [
    ...pageIssues,
    ...validateReferences([page], codes)
  ];
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = runPreCheck();
  reportPreCheck(result);
  if (!result.ok) process.exit(1);
}
