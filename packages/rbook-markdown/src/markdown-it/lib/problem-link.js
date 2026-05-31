import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';

const configCache = new Map();

function stripTrailingSlash(url) {
  return String(url || '').replace(/\/+$/, '');
}

function normalizeProblemId(oj, id) {
  const raw = String(id || '').trim();
  if (String(oj || '').toLowerCase() === 'luogu' && /^\d+$/.test(raw)) {
    return `P${raw}`;
  }

  return raw.replace(/^([PBTU])(\d+)$/i, (match, prefix, number) => {
    return `${prefix.toUpperCase()}${number}`;
  });
}

function findConfig(startFilePath) {
  let currentDir = startFilePath ? path.dirname(startFilePath) : process.cwd();

  while (currentDir && currentDir !== path.dirname(currentDir)) {
    for (const filename of ['config.yaml', 'config.yml', 'book.yaml', 'book.yml']) {
      const configPath = path.join(currentDir, filename);
      if (fs.existsSync(configPath)) {
        return configPath;
      }
    }
    currentDir = path.dirname(currentDir);
  }

  return null;
}

function loadConfig(configPath) {
  if (!configPath) return {};
  if (configCache.has(configPath)) return configCache.get(configPath);

  const raw = fs.readFileSync(configPath, 'utf8');
  const config = yaml.load(raw) || {};
  configCache.set(configPath, config);
  return config;
}

function getPcsLink(env, options) {
  if (env?.pcsLink) return stripTrailingSlash(env.pcsLink);
  if (options.pcsLink) return stripTrailingSlash(options.pcsLink);

  const configPath = options.configPath || findConfig(env?.filePath);
  const config = loadConfig(configPath);
  return stripTrailingSlash(config['pcs-link'] || options.defaultPcsLink || '');
}

function buildProblemHref(baseUrl, oj, id) {
  const normalizedBase = stripTrailingSlash(baseUrl);
  const encodedOj = encodeURIComponent(oj);
  const encodedId = encodeURIComponent(id);

  if (!normalizedBase) {
    return `/problems/${encodedOj}/${encodedId}`;
  }

  return `${normalizedBase}/problems/${encodedOj}/${encodedId}`;
}

function problemLinkPlugin(md, options = {}) {
  md.inline.ruler.after('text', 'problem_link', function problemLinkRule(state, silent) {
    const pos = state.pos;
    const max = state.posMax;

    if (pos + 11 >= max || state.src.slice(pos, pos + 10) !== '[[problem:') {
      return false;
    }

    const endPos = state.src.indexOf(']]', pos + 10);
    if (endPos === -1) {
      return false;
    }

    const content = state.src.slice(pos + 10, endPos);
    const match = content.match(/^\s*([^,\]]+),\s*([^,\]]+)\s*$/);
    if (!match) {
      return false;
    }

    if (silent) {
      state.pos = endPos + 2;
      return true;
    }

    const oj = match[1].trim();
    const id = normalizeProblemId(oj, match[2]);
    const href = buildProblemHref(getPcsLink(state.env, options), oj, id);

    const token = state.push('link_open', 'a', 1);
    token.attrSet('href', href);
    token.attrSet('class', 'problem-link');
    token.attrSet('target', '_blank');
    token.attrSet('rel', 'noopener');

    const textToken = state.push('text', '', 0);
    textToken.content = `${oj}-${id}`;

    state.push('link_close', 'a', -1);
    state.pos = endPos + 2;
    return true;
  });
}

export default problemLinkPlugin;
