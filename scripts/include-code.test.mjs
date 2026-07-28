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
