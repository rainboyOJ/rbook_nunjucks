import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import { expandIncludeCode } from '@rbook/markdown/include-code';
import { codeTemplateDir, contentDir } from './paths.js';

export interface CodeTemplateItem {
  id: string;
  path: string;
  description: string;
  language?: string;
  tags?: string[];
  complexity?: string;
  author?: string;
  aliases?: string[];
  [key: string]: unknown;
}

export interface CodeConfig {
  codes: CodeTemplateItem[];
}

export interface LoadCodeConfigOptions {
  codeYamlPath?: string;
  strict?: boolean;
}

export interface ValidationError {
  level: 'ERROR' | 'WARNING';
  filePath: string;
  message: string;
}

export const PUBLIC_ID_PATTERN = /^[a-z0-9-]+$/;

export type PublicIdError = 'type' | 'empty' | 'format';

export type PublicIdResult =
  | { ok: true; id: string }
  | { ok: false; error: PublicIdError };

export function parsePublicId(value: unknown): PublicIdResult {
  if (typeof value !== 'string') return { ok: false, error: 'type' };
  if (value.trim().length === 0) return { ok: false, error: 'empty' };
  if (!PUBLIC_ID_PATTERN.test(value)) return { ok: false, error: 'format' };
  return { ok: true, id: value };
}

function pageIdValue(page: any): unknown {
  const frontMatter = page?.frontMatter;
  if (frontMatter && Object.prototype.hasOwnProperty.call(frontMatter, 'id')) {
    return frontMatter.id;
  }
  return page?.id;
}

function publicIdErrorMessage(
  result: Extract<PublicIdResult, { ok: false }>,
  kind: 'page' | 'code',
  value: unknown
) {
  const label = kind === 'page' ? '文章' : '代码';
  if (result.error === 'type') return `${label} ID 必须是字符串`;
  if (result.error === 'empty') return `missing required field 'id'`;
  return `${kind} id '${String(value)}' does not match pattern ${PUBLIC_ID_PATTERN}`;
}

export function requirePageId(page: any): string {
  const filePath = String(page?.path || 'unknown');
  const value = pageIdValue(page);
  const result = parsePublicId(value);
  if (result.ok === false) {
    throw new Error(`${filePath}: ${publicIdErrorMessage(result, 'page', value)}`);
  }
  return result.id;
}

export function requireCodeId(code: any, filePath = 'book/code.yaml'): string {
  const value = code?.id;
  const result = parsePublicId(value);
  if (result.ok === false) {
    throw new Error(`${filePath}: ${publicIdErrorMessage(result, 'code', value)}`);
  }
  return result.id;
}

function invalidCodeConfig(
  codeYamlPath: string,
  message: string,
  strict: boolean
): CodeConfig {
  if (strict) {
    throw new Error(`invalid code config '${codeYamlPath}': ${message}`);
  }
  return { codes: [] };
}

export function loadCodeConfig(options: LoadCodeConfigOptions = {}): CodeConfig {
  const codeYamlPath = options.codeYamlPath
    ? path.resolve(options.codeYamlPath)
    : path.join(contentDir, 'code.yaml');
  const strict = options.strict === true;

  if (!fs.existsSync(codeYamlPath)) {
    return invalidCodeConfig(codeYamlPath, 'file does not exist', strict);
  }

  let data: unknown;
  try {
    const content = fs.readFileSync(codeYamlPath, 'utf8');
    data = yaml.load(content);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`failed to load code config '${codeYamlPath}': ${message}`);
  }

  if (!data || typeof data !== 'object' || Array.isArray(data)) {
    return invalidCodeConfig(codeYamlPath, 'root must be a YAML mapping', strict);
  }

  const codes = (data as { codes?: unknown }).codes;
  if (!Array.isArray(codes)) {
    return invalidCodeConfig(codeYamlPath, "field 'codes' must be an array", strict);
  }
  if (strict && codes.some((item) => !item || typeof item !== 'object' || Array.isArray(item))) {
    return invalidCodeConfig(codeYamlPath, "every item in 'codes' must be a mapping", true);
  }

  return { codes: codes as CodeTemplateItem[] };
}

export function validateCodes(codes: CodeTemplateItem[]): ValidationError[] {
  const errors: ValidationError[] = [];
  const seenIds = new Set<string>();

  for (const item of codes) {
    const itemPath = item.path || 'unknown';
    const idResult = parsePublicId(item.id);
    if (idResult.ok === false) {
      errors.push({
        level: 'ERROR',
        filePath: `book/code.yaml`,
        message: idResult.error === 'empty'
          ? `code entry with path '${itemPath}' is missing required field 'id'`
          : publicIdErrorMessage(idResult, 'code', item.id)
      });
    } else if (seenIds.has(idResult.id)) {
      errors.push({
        level: 'ERROR',
        filePath: `book/code.yaml`,
        message: `duplicate code id '${idResult.id}'`
      });
    } else {
      seenIds.add(idResult.id);
    }

    if (!item.path) {
      errors.push({
        level: 'ERROR',
        filePath: `book/code.yaml`,
        message: `code entry '${item.id}' is missing required field 'path'`
      });
    } else {
      const fullPath = path.resolve(codeTemplateDir, item.path);
      if (!fs.existsSync(fullPath)) {
        errors.push({
          level: 'ERROR',
          filePath: `book/code.yaml`,
          message: `code file '${item.path}' for id '${item.id}' does not exist on disk`
        });
      }
    }

    if (!item.description) {
      errors.push({
        level: 'WARNING',
        filePath: `book/code.yaml`,
        message: `code entry '${item.id}' is missing field 'description'`
      });
    }
  }

  return errors;
}

export function validatePages(pages: any[]): ValidationError[] {
  const errors: ValidationError[] = [];
  const seenIds = new Map<string, string>();

  for (const page of pages) {
    const filePath = page.path || 'unknown';
    const fm = page.frontMatter || {};

    const id = pageIdValue(page);
    const idResult = parsePublicId(id);
    if (idResult.ok === false) {
      errors.push({
        level: 'ERROR',
        filePath,
        message: publicIdErrorMessage(idResult, 'page', id)
      });
    } else {
      const existingPath = seenIds.get(idResult.id);
      if (existingPath) {
        errors.push({
          level: 'ERROR',
          filePath,
          message: `duplicate id '${idResult.id}' with ${existingPath}`
        });
      } else {
        seenIds.set(idResult.id, filePath);
      }
    }

    const title = fm.title || page.title;
    if (!title) {
      errors.push({
        level: 'ERROR',
        filePath,
        message: `missing required field 'title'`
      });
    }

    if (!fm.description && !fm.desc) {
      errors.push({
        level: 'WARNING',
        filePath,
        message: `missing field 'description'`
      });
    }

    if (!fm.tags) {
      errors.push({
        level: 'WARNING',
        filePath,
        message: `missing field 'tags'`
      });
    } else if (!Array.isArray(fm.tags)) {
      errors.push({
        level: 'ERROR',
        filePath,
        message: `'tags' must be an array`
      });
    }
  }

  return errors;
}

export function validateReferences(pages: any[], codes: CodeTemplateItem[]): ValidationError[] {
  const errors: ValidationError[] = [];
  const validCodeIds = new Set(
    codes
      .map((code) => parsePublicId(code.id))
      .filter((result): result is Extract<PublicIdResult, { ok: true }> => result.ok)
      .map((result) => result.id)
  );
  const codesById = new Map(
    codes
      .filter((code) => typeof code?.id === 'string')
      .map((code) => [code.id, code])
  );

  for (const page of pages) {
    const filePath = page.path || 'unknown';
    const fm = page.frontMatter || {};
    const codeTemplates = fm.code_template;

    if (codeTemplates !== undefined && codeTemplates !== null) {
      if (!Array.isArray(codeTemplates)) {
        errors.push({
          level: 'ERROR',
          filePath,
          message: `'code_template' field must be an array of code IDs`
        });
        continue;
      }

      for (const refId of codeTemplates) {
        if (typeof refId !== 'string') {
          errors.push({
            level: 'ERROR',
            filePath,
            message: `'code_template' item must be a string ID, got ${typeof refId}`
          });
          continue;
        }

        if (!validCodeIds.has(refId)) {
          errors.push({
            level: 'ERROR',
            filePath,
            message: `referenced code_template ID '${refId}' is not registered in book/code.yaml`
          });
        }
      }
    }

    const source = page.sourceContent;
    if (typeof source === 'string') {
      const expanded = expandIncludeCode(source, {
        baseDir: contentDir,
        codeDir: codeTemplateDir,
        currentFilePath: path.join(contentDir, 'pages', filePath),
        resolveCodeId: (id) => codesById.get(id) || null
      });
      const includeErrors = /<!-- include-code error: ([\s\S]*?): (path is outside the content directory|referenced code was not found) -->/g;
      for (const match of expanded.matchAll(includeErrors)) {
        errors.push({
          level: 'ERROR',
          filePath,
          message: `@include-code '${match[1]}' failed: ${match[2]}`
        });
      }
    }
  }

  return errors;
}
