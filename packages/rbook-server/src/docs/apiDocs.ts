interface ApiEndpointDoc {
  method: 'GET' | 'POST';
  path: string;
  description: string;
  query: string;
  example: string;
  note?: string;
}

export const apiEndpointDocs: ApiEndpointDoc[] = [
  {
    method: 'GET',
    path: '/api/health',
    description: '服务健康检查，返回索引生成时间和页面/chunk 统计。',
    query: '-',
    example: 'curl "$BASE_URL/api/health"'
  },
  {
    method: 'GET',
    path: '/api/site',
    description: '返回电子书站点元信息、统计信息和索引生成时间。',
    query: '-',
    example: 'curl "$BASE_URL/api/site"'
  },
  {
    method: 'GET',
    path: '/api/toc',
    description: '返回电子书目录树，适合定位主题在书中的位置。',
    query: '-',
    example: 'curl "$BASE_URL/api/toc"'
  },
  {
    method: 'GET',
    path: '/api/nav',
    description: '返回和 /api/toc 相同的导航树，保留给导航语义调用。',
    query: '-',
    example: 'curl "$BASE_URL/api/nav"'
  },
  {
    method: 'GET',
    path: '/api/pages',
    description: '返回页面元数据列表。',
    query: 'visible=true 可选，只返回目录中可见页面',
    example: 'curl "$BASE_URL/api/pages?visible=true"'
  },
  {
    method: 'GET',
    path: '/api/page',
    description: '返回单页详情，包括 front matter、headings、markdown、html、text 和 chunks。',
    query: 'path 必填，例如 dynamic_programming/数位DP/index.md',
    example: 'curl "$BASE_URL/api/page?path=dynamic_programming/%E6%95%B0%E4%BD%8DDP/index.md"'
  },
  {
    method: 'GET',
    path: '/api/search',
    description: '页面级全文搜索，适合先找到相关页面。',
    query: 'q 必填；limit 可选，最大 50',
    example: 'curl -G --data-urlencode "q=数位DP" --data-urlencode "limit=5" "$BASE_URL/api/search"'
  },
  {
    method: 'GET',
    path: '/api/chunks/search',
    description: 'chunk 级搜索，适合 agent 获取更聚焦的上下文。',
    query: 'q 必填；limit、textLength、includeText 可选',
    example: 'curl -G --data-urlencode "q=数位DP 状态 记忆化" --data-urlencode "limit=8" --data-urlencode "textLength=900" "$BASE_URL/api/chunks/search"'
  },
  {
    method: 'GET',
    path: '/api/ai/catalog',
    description: '返回 AI 友好的正式文章目录，包含文章链接、摘要、标签和 code_template 元数据。',
    query: 'scope=visible|all 可选，默认 visible',
    example: 'curl "$BASE_URL/api/ai/catalog"'
  },
  {
    method: 'GET',
    path: '/api/ai/page-context',
    description: '返回 AI 解题所需的单页上下文，可包含完整文章、引用信息、模板代码和正文 include-code 代码。',
    query: 'path 必填；includeCode=true 可选；includeHtml=true 可选',
    example: 'curl "$BASE_URL/api/ai/page-context?path=graph/bcc/index.md&includeCode=true"'
  },
  {
    method: 'GET',
    path: '/api/ai/code',
    description: '按 /code/... 路径读取模板代码内容，只允许访问本项目 book/code 下的文件。',
    query: 'path 必填，例如 /code/graph/v-bcc.cpp',
    example: 'curl "$BASE_URL/api/ai/code?path=/code/graph/v-bcc.cpp"'
  },
  {
    method: 'POST',
    path: '/api/admin/reindex',
    description: '重建搜索索引。',
    query: '-',
    example: 'curl -X POST -H "Authorization: Bearer $RBOOK_ADMIN_TOKEN" "$BASE_URL/api/admin/reindex"',
    note: '生产环境设置 RBOOK_ADMIN_TOKEN 后，需要 Bearer token 或 x-rbook-token。'
  }
];

function escapeHtml(value: string) {
  return value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}

export function renderApiDocsPage(baseUrl: string) {
  const endpointHtml = apiEndpointDocs.map((endpoint) => `
    <section class="endpoint">
      <div class="endpoint-title">
        <span class="method ${endpoint.method.toLowerCase()}">${endpoint.method}</span>
        <code>${escapeHtml(endpoint.path)}</code>
      </div>
      <p>${escapeHtml(endpoint.description)}</p>
      <dl>
        <dt>Query</dt>
        <dd><code>${escapeHtml(endpoint.query)}</code></dd>
        <dt>Example</dt>
        <dd><pre><code>${escapeHtml(endpoint.example.replace(/\$BASE_URL/g, baseUrl))}</code></pre></dd>
        ${endpoint.note ? `<dt>Note</dt><dd>${escapeHtml(endpoint.note)}</dd>` : ''}
      </dl>
    </section>
  `).join('');

  return `<!doctype html>
<html lang="zh">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>rbook API 文档</title>
  <style>
    :root {
      color-scheme: light dark;
      --bg: #f7f7f4;
      --panel: #ffffff;
      --text: #1f2933;
      --muted: #5d6978;
      --border: #d9dee5;
      --code: #eef2f6;
      --accent: #16745f;
      --post: #7a4d12;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #151719;
        --panel: #1f2328;
        --text: #e6edf3;
        --muted: #a6b0bd;
        --border: #3a424c;
        --code: #2b3138;
      }
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      background: var(--bg);
      color: var(--text);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
      line-height: 1.65;
    }
    main {
      width: min(980px, calc(100% - 32px));
      margin: 0 auto;
      padding: 42px 0 64px;
    }
    header {
      margin-bottom: 26px;
      border-bottom: 1px solid var(--border);
      padding-bottom: 20px;
    }
    h1 {
      margin: 0 0 8px;
      font-size: clamp(28px, 5vw, 42px);
      line-height: 1.2;
    }
    h2 {
      margin: 30px 0 12px;
      font-size: 22px;
    }
    p { margin: 0 0 12px; }
    .muted { color: var(--muted); }
    .base-url {
      display: inline-block;
      margin-top: 8px;
      padding: 8px 10px;
      border: 1px solid var(--border);
      border-radius: 6px;
      background: var(--panel);
    }
    .endpoint {
      background: var(--panel);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 18px;
      margin: 14px 0;
    }
    .endpoint-title {
      display: flex;
      gap: 10px;
      align-items: center;
      flex-wrap: wrap;
      margin-bottom: 8px;
    }
    .method {
      min-width: 54px;
      text-align: center;
      border-radius: 999px;
      padding: 3px 9px;
      color: #fff;
      background: var(--accent);
      font-size: 13px;
      font-weight: 700;
    }
    .method.post { background: var(--post); }
    code, pre {
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
      font-size: 0.93em;
    }
    code {
      background: var(--code);
      border-radius: 4px;
      padding: 2px 5px;
    }
    pre {
      overflow-x: auto;
      margin: 0;
      padding: 10px;
      background: var(--code);
      border-radius: 6px;
    }
    pre code {
      padding: 0;
      background: transparent;
    }
    dl {
      display: grid;
      grid-template-columns: 90px minmax(0, 1fr);
      gap: 8px 12px;
      margin: 10px 0 0;
    }
    dt {
      color: var(--muted);
      font-weight: 700;
    }
    dd { margin: 0; min-width: 0; }
    ul { padding-left: 22px; }
    a { color: var(--accent); }
    @media (max-width: 640px) {
      main { width: min(100% - 20px, 980px); padding-top: 26px; }
      .endpoint { padding: 14px; }
      dl { grid-template-columns: 1fr; }
    }
  </style>
</head>
<body>
  <main>
    <header>
      <h1>rbook API 文档</h1>
      <p class="muted">这个服务只提供算法电子书内容检索和读取 API，不提供 chat，也不在站点内嵌入 AI。</p>
      <div class="base-url"><strong>BASE_URL</strong> <code>${escapeHtml(baseUrl)}</code></div>
    </header>

    <section>
      <h2>推荐调用流程</h2>
      <ul>
        <li>AI 解题或生成题解时，先用 <code>/api/ai/catalog</code> 获取正式文章和模板目录。</li>
        <li>确定页面后，用 <code>/api/ai/page-context?path=...&amp;includeCode=true</code> 获取完整文章和模板代码。</li>
        <li>只需要模板代码时，用 <code>/api/ai/code?path=/code/...</code> 读取代码内容。</li>
        <li>普通搜索仍可使用 <code>/api/chunks/search</code> 或 <code>/api/search</code>。</li>
        <li>本项目不提供题目数据查询；题目数据已经拆到独立服务。</li>
      </ul>
    </section>

    <section>
      <h2>Endpoints</h2>
      ${endpointHtml}
    </section>
  </main>
</body>
</html>`;
}
