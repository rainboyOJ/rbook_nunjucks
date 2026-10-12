import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { expandIncludeCode } from '@rbook/markdown/include-code';
import { render } from '@rbook/markdown/markdown-it';

test('include-code expands paths and template IDs into standalone Markdown', (t) => {
  const contentDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-include-code-'));
  t.after(() => fs.rmSync(contentDir, { recursive: true, force: true }));

  const pageDir = path.join(contentDir, 'pages/example');
  const codeDir = path.join(contentDir, 'code');
  fs.mkdirSync(pageDir, { recursive: true });
  fs.mkdirSync(codeDir, { recursive: true });
  fs.writeFileSync(path.join(codeDir, 'main.cpp'), 'int main() {}\n```\n');
  fs.writeFileSync(path.join(codeDir, 'template.py'), 'print("template")\n');
  fs.writeFileSync(path.join(pageDir, 'input.txt'), '1 2 3\n');

  const source = `---
id: example
directive: "@include-code(/code/ignored.cpp, cpp)"
---

@include-code(/code/main.cpp, cpp)

@include-code(./input.txt)

@include-code(template-id)

\`\`\`bash
@include-code(./input.txt)
\`\`\`

\`\`\`text
literal directive example:
@include-code(/code/main.cpp, cpp)
\`\`\`
`;

  const expanded = expandIncludeCode(source, {
    baseDir: contentDir,
    codeDir,
    currentFilePath: path.join(pageDir, 'index.md'),
    resolveCodeId: (id) => id === 'template-id'
      ? { path: 'template.py', language: 'python' }
      : null
  });

  assert.match(expanded, /^---\nid: example\ndirective: "@include-code\(\/code\/ignored\.cpp, cpp\)"\n---/);
  assert.match(expanded, /~~~cpp\nint main\(\) \{\}\n```\n~~~/);
  assert.match(expanded, /```txt\n1 2 3\n```/);
  assert.match(expanded, /```python\nprint\("template"\)\n```/);
  assert.match(expanded, /```bash\n1 2 3\n```/);
  assert.match(expanded, /```text\nliteral directive example:\n@include-code\(\/code\/main\.cpp, cpp\)\n```/);
});

test('include-code reports missing and unsafe references without leaking files', (t) => {
  const contentDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-include-code-safe-'));
  t.after(() => fs.rmSync(contentDir, { recursive: true, force: true }));

  const pageDir = path.join(contentDir, 'pages/example');
  fs.mkdirSync(pageDir, { recursive: true });
  const source = '@include-code(../../../secret.txt, text)\n@include-code(missing-id)\n';
  const expanded = expandIncludeCode(source, {
    baseDir: contentDir,
    currentFilePath: path.join(pageDir, 'index.md'),
    resolveCodeId: () => null
  });

  assert.match(expanded, /include-code error: \.\.\/\.\.\/\.\.\/secret\.txt: path is outside/);
  assert.match(expanded, /include-code error: missing-id: referenced code was not found/);
  assert.equal(expanded.includes(contentDir), false);
});

test('line-numbered code keeps internal blank lines but removes trailing blank lines', () => {
  const html = render('```cpp\nint first;\n\nint second;\n\n\n```\n');
  const lineNumbers = html.match(/class="line-number"/g) || [];

  assert.equal(lineNumbers.length, 3);
  assert.match(html, /int first/);
  assert.match(html, /int second/);
});

test('include-code keeps fences clean unless findCodeId maps the file to a template ID', (t) => {
  const contentDir = fs.mkdtempSync(path.join(os.tmpdir(), 'rbook-include-code-id-'));
  t.after(() => fs.rmSync(contentDir, { recursive: true, force: true }));

  const pageDir = path.join(contentDir, 'pages/example');
  const codeDir = path.join(contentDir, 'code');
  fs.mkdirSync(pageDir, { recursive: true });
  fs.mkdirSync(codeDir, { recursive: true });
  fs.writeFileSync(path.join(codeDir, 'main.cpp'), 'int main() {}\n');
  fs.writeFileSync(path.join(codeDir, 'template.py'), 'print("template")\n');
  fs.writeFileSync(path.join(pageDir, 'local.txt'), 'local\n');

  const source = [
    '@include-code(/code/main.cpp, cpp)',
    '@include-code(template-id)',
    '@include-code(./local.txt, text)'
  ].join('\n\n');

  const expanded = expandIncludeCode(source, {
    baseDir: contentDir,
    codeDir,
    currentFilePath: path.join(pageDir, 'index.md'),
    resolveCodeId: (id) => id === 'template-id'
      ? { path: 'template.py', language: 'python' }
      : null,
    findCodeId: (filePath) => {
      if (filePath === path.join(codeDir, 'main.cpp')) return 'main-code';
      if (filePath === path.join(codeDir, 'template.py')) return 'template-id';
      return null;
    }
  });

  // 模板文件带上 ID 标记；文章自己的一次性文件保持干净围栏。
  assert.match(expanded, /```cpp rbook-code-template:main-code\nint main\(\) \{\}\n```/);
  assert.match(expanded, /```python rbook-code-template:template-id\nprint\("template"\)\n```/);
  assert.match(expanded, /```text\nlocal\n```/);
  assert.equal(expanded.includes('rbook-code-template:local'), false);
});

test('fence renderer strips the template marker and exposes data-code-template-id', () => {
  const numbered = render('```cpp rbook-code-template:dsu\nint x;\n```\n');
  assert.match(numbered, /<div data-code-template-id="dsu" class="language-clike line-numbers-mode">/);
  assert.equal(numbered.includes('rbook-code-template'), false);

  // 不走行号模板的兜底 fence 渲染也要带同样的 data 属性。
  const plain = render('```rust rbook-code-template:rs-template\nfn main() {}\n```\n');
  assert.match(plain, /<pre data-code-template-id="rs-template">/);
  assert.equal(plain.includes('rbook-code-template'), false);
});
