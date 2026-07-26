import assert from 'node:assert/strict';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const clientPath = path.join(rootDir, 'scripts/rbook.py');
const skillClientPath = path.join(rootDir, '.agents/skills/rbook-http/scripts/rbook.py');

function runPython(args) {
  return spawnSync('python3', args, {
    cwd: rootDir,
    encoding: 'utf8'
  });
}

test('find requires every word and ranks id or title before tags and description', () => {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)

pages = [
    {"id": "other", "title": "Other", "description": "kmp 字符串", "tags": []},
    {"id": "kmp-algo", "title": "KMP", "description": "字符串匹配", "tags": ["字符串"]},
    {"id": "kmp-only", "title": "KMP only", "description": "prefix", "tags": []},
]
print(json.dumps(client.find_pages(pages, "kmp 字符串", 20), ensure_ascii=False))
`;
  const result = runPython(['-c', program]);
  assert.equal(result.status, 0, result.stderr);
  const payload = JSON.parse(result.stdout);
  assert.equal(payload.total, 2);
  assert.deepEqual(payload.items.map((item) => item.id), ['kmp-algo', 'other']);
});

test('find applies its result limit after counting all matches', () => {
  const program = `
import importlib.util
import json

spec = importlib.util.spec_from_file_location("rbook_client", ${JSON.stringify(clientPath)})
client = importlib.util.module_from_spec(spec)
spec.loader.exec_module(client)
pages = [{"id": f"page-{index}", "title": "Graph", "description": "", "tags": []} for index in range(25)]
print(json.dumps(client.find_pages(pages, "graph", 20)))
`;
  const result = runPython(['-c', program]);
  assert.equal(result.status, 0, result.stderr);
  const payload = JSON.parse(result.stdout);
  assert.equal(payload.total, 25);
  assert.equal(payload.items.length, 20);
});

test('the skill entry delegates to the canonical client', () => {
  const canonical = runPython([clientPath, '--help']);
  const skill = runPython([skillClientPath, '--help']);
  assert.equal(canonical.status, 0, canonical.stderr);
  assert.equal(skill.status, 0, skill.stderr);
  assert.equal(skill.stdout, canonical.stdout);
  assert.match(skill.stdout, /\bfind\b/);
});
