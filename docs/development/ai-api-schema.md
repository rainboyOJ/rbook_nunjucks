# AI API 字段契约

本文档描述 `/api/ai/*` 接口给本地智能体使用的稳定字段。AI API 只返回电子书文章、引用信息和模板代码，不提供题目数据，也不提供 chat 功能。

## 链接规则

- AI API 不返回服务端拼接的绝对链接。
- 响应体中不应出现 `href`、`codeHref` 这类旧字段。
- 响应体中不应出现 `127.0.0.1` 这类本地服务地址。
- `url`、`codeUrl` 都是以 `/` 开头的站内相对路径。
- 需要可点击链接时，由调用方使用 `BASE_URL + url` 或 `BASE_URL + codeUrl` 拼接。

## GET /api/ai/catalog

返回文章目录：

```ts
{
  scope: 'visible' | 'all';
  total: number;
  generatedAt: string;
  articles: AiCatalogItem[];
}
```

`AiCatalogItem`：

```ts
{
  path: string;              // Markdown 路径，例如 utils/random/index.md
  url: string;               // 页面相对链接，例如 /utils/random/index.html
  title: string;
  description: string;
  excerpt: string;
  tags: string[];
  categories: string[];
  headings: unknown[];
  navTrail: unknown[];
  visible: boolean;
  source: string;
  codeTemplates: AiCodeTemplate[];
  citation: {
    title: string;
    path: string;
    url: string;
  };
}
```

## GET /api/ai/page-context

返回单篇文章上下文：

```ts
{
  article: {
    path: string;
    url: string;
    title: string;
    description: string;
    excerpt: string;
    visible: boolean;
    source: string;
    navTrail: unknown[];
    tags: string[];
    categories: string[];
    frontMatter: Record<string, unknown>;
    headings: unknown[];
    markdown: string;
    text: string;
    html?: string;           // includeHtml=true 时返回
    chunks: unknown[];
    citation: {
      title: string;
      path: string;
      url: string;
    };
  };
  codeTemplates: AiCodeTemplate[];
  includedCode: AiIncludedCode[];
}
```

`AiCodeTemplate`：

```ts
{
  source: 'frontMatter';
  title: string;
  desc: string;
  tags: string[];
  code: string;              // /code/... 路径
  codeUrl: string;           // /code/... 相对路径
  language: string;
  content?: string;          // includeCode=true 时返回
}
```

`AiIncludedCode`：

```ts
{
  source: 'include-code';
  path: string;
  code: string;              // 原始 @include-code 参数
  codeUrl: string | null;    // /code/... 才有可拼接链接
  language: string;
  content?: string;          // includeCode=true 时返回
  error?: string;
}
```

## GET /api/ai/code

读取 `book/code/` 下单个模板代码：

```ts
{
  path: string;              // /code/... 路径
  url: string;               // /code/... 相对路径
  language: string;
  content: string;
}
```

`path` 只接受 `/code/...` 或 `code/...`，不能使用 `../` 读取任意文件。
