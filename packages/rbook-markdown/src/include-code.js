import fs from 'fs';
import path from 'path';

const directivePattern = /^([ \t]*)@include-code\(\s*([^,)]+?)\s*(?:,\s*([^)]+?)\s*)?\)\s*$/;

export function parseIncludeCodeDirective(line) {
  const match = String(line).match(directivePattern);
  if (!match) return null;

  return {
    indent: match[1],
    reference: match[2].trim(),
    language: (match[3] || '').trim()
  };
}

function isInside(root, candidate) {
  const relative = path.relative(path.resolve(root), path.resolve(candidate));
  return relative === '' || (!relative.startsWith('..') && !path.isAbsolute(relative));
}

function isFile(filePath) {
  try {
    return fs.statSync(filePath).isFile();
  } catch {
    return false;
  }
}

function inferLanguage(filePath) {
  return path.extname(filePath).replace(/^\./, '').toLowerCase() || 'text';
}

function normalizeLanguage(language, filePath) {
  const value = String(language || inferLanguage(filePath)).trim().split(/\s+/, 1)[0];
  return value.replace(/[^a-zA-Z0-9_+.-]/g, '') || 'text';
}

function directCandidates(reference, baseDir, currentFilePath) {
  if (reference.startsWith('/')) {
    return [path.resolve(baseDir, `.${reference}`)];
  }

  const candidates = [path.resolve(path.dirname(currentFilePath), reference)];
  if (reference.startsWith('code/')) {
    candidates.push(path.resolve(baseDir, reference));
  }
  return candidates;
}

function codeIdCandidate(resolved, baseDir, codeDir) {
  if (!resolved) return null;
  const item = typeof resolved === 'string' ? { path: resolved } : resolved;
  if (!item.path || typeof item.path !== 'string') return null;

  let filePath;
  if (item.path.startsWith('/')) {
    filePath = path.resolve(baseDir, `.${item.path}`);
  } else if (item.path.startsWith('code/')) {
    filePath = path.resolve(baseDir, item.path);
  } else {
    filePath = path.resolve(codeDir, item.path);
  }

  return { filePath, language: item.language || '' };
}

export function readIncludedCode(reference, options = {}) {
  const baseDir = path.resolve(options.baseDir || '.');
  const codeDir = path.resolve(options.codeDir || path.join(baseDir, 'code'));
  const currentFilePath = path.resolve(options.currentFilePath || path.join(baseDir, 'index.md'));
  let rejectedUnsafePath = false;

  for (const candidate of directCandidates(reference, baseDir, currentFilePath)) {
    if (!isInside(baseDir, candidate)) {
      rejectedUnsafePath = true;
      continue;
    }
    if (isFile(candidate)) {
      return {
        content: fs.readFileSync(candidate, 'utf8'),
        language: normalizeLanguage(options.language, candidate)
      };
    }
  }

  if (typeof options.resolveCodeId === 'function') {
    const resolved = codeIdCandidate(options.resolveCodeId(reference), baseDir, codeDir);
    if (resolved) {
      if (!isInside(baseDir, resolved.filePath)) {
        rejectedUnsafePath = true;
      } else if (isFile(resolved.filePath)) {
        return {
          content: fs.readFileSync(resolved.filePath, 'utf8'),
          language: normalizeLanguage(options.language || resolved.language, resolved.filePath)
        };
      }
    }
  }

  return {
    error: rejectedUnsafePath
      ? 'path is outside the content directory'
      : 'referenced code was not found'
  };
}

function longestRun(content, marker) {
  let longest = 0;
  let current = 0;
  for (const character of content) {
    if (character === marker) {
      current += 1;
      longest = Math.max(longest, current);
    } else {
      current = 0;
    }
  }
  return longest;
}

function fencedCode(content, language, indent) {
  const normalized = String(content).replace(/\r\n?/g, '\n').replace(/\n*$/, '');
  const backticks = '`'.repeat(Math.max(3, longestRun(normalized, '`') + 1));
  const tildes = '~'.repeat(Math.max(3, longestRun(normalized, '~') + 1));
  const fence = backticks.length <= tildes.length ? backticks : tildes;
  const body = normalized
    ? normalized.split('\n').map((line) => `${indent}${line}`).join('\n')
    : indent;
  return `${indent}${fence}${language}\n${body}\n${indent}${fence}`;
}

function errorComment(reference, message, indent) {
  const safeReference = String(reference).replace(/[\r\n]/g, ' ').replace(/--/g, '- -');
  return `${indent}<!-- include-code error: ${safeReference}: ${message} -->`;
}

function openingFence(line) {
  const match = line.match(/^( {0,3})(`{3,}|~{3,})(.*)$/);
  return match
    ? {
        indent: match[1],
        marker: match[2][0],
        length: match[2].length,
        language: match[3].trim().split(/\s+/, 1)[0] || ''
      }
    : null;
}

function closesFence(line, fence) {
  const match = line.match(/^ {0,3}(`+|~+)\s*$/);
  return Boolean(match && match[1][0] === fence.marker && match[1].length >= fence.length);
}

function closingFenceIndex(lines, startIndex, fence) {
  for (let index = startIndex + 1; index < lines.length; index += 1) {
    if (closesFence(lines[index], fence)) return index;
  }
  return -1;
}

function expandDirective(directive, options, fallbackLanguage = '', fallbackIndent = '') {
  const included = readIncludedCode(directive.reference, {
    ...options,
    language: directive.language || fallbackLanguage
  });
  const indent = fallbackIndent || directive.indent;
  return included.error
    ? errorComment(directive.reference, included.error, indent)
    : fencedCode(included.content, included.language, indent);
}

export function expandIncludeCode(source, options = {}) {
  const lines = String(source).replace(/\r\n?/g, '\n').split('\n');
  const output = [];
  let fence = null;
  let inFrontMatter = lines[0]?.replace(/^\uFEFF/, '') === '---';

  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index];

    if (inFrontMatter) {
      output.push(line);
      if (index > 0 && (line === '---' || line === '...')) inFrontMatter = false;
      continue;
    }

    if (fence) {
      output.push(line);
      if (closesFence(line, fence)) fence = null;
      continue;
    }

    const nextFence = openingFence(line);
    if (nextFence) {
      const closeIndex = closingFenceIndex(lines, index, nextFence);
      if (closeIndex !== -1) {
        const meaningfulLines = lines
          .slice(index + 1, closeIndex)
          .filter((item) => item.trim().length > 0);
        const wrappedDirective = meaningfulLines.length === 1
          ? parseIncludeCodeDirective(meaningfulLines[0])
          : null;
        if (wrappedDirective) {
          output.push(expandDirective(
            wrappedDirective,
            options,
            nextFence.language,
            nextFence.indent
          ));
          index = closeIndex;
          continue;
        }
      }

      fence = nextFence;
      output.push(line);
      continue;
    }

    const directive = parseIncludeCodeDirective(line);
    if (!directive) {
      output.push(line);
      continue;
    }

    output.push(expandDirective(directive, options));
  }

  return output.join('\n');
}
