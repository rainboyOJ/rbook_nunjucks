# Rbook HTTP API 字段契约（优化版）

本文档描述提供给本地智能体和前端客户端使用的最新稳定 HTTP API 字段契约。

## 设计约定与规范
1. **主键查询约定**：所有资源（文章、代码）都有全局唯一的 `id`，通过 `?id=<id>` 精确查询单资源；通过 `?tag=<tag1,tag2>` 筛选集合。
2. **链接与地址**：API 不返回含域名或端口的绝对 URL。所有的 `url`、`path` 字段均为站内相对路径或相对根目录路径。客户端需通过 `BASE_URL + url` 拼接完整地址。
3. **元数据集中管理**：代码模板信息由 `book/code.yaml` 集中管理；文章 frontMatter 中的 `code_template` 只存放对应模板的 ID 数组。

---

## 1. GET /api/catalog
返回文章目录与基本结构。支持使用 `?compact=true` 返回极简数据，优化 Token 消耗。

**请求示例**：
```bash
curl "$BASE_URL/api/catalog?compact=true"
```

**响应契约**：
```ts
{
  generatedAt: string;
  total: number;
  items: Array<{
    id: string;               // 唯一文章标识符
    title: string;            // 文章标题
    description: string;      // 描述或摘要
    tags: string[];           // 标签列表
    path: string;             // Markdown 文件路径
    url: string;              // HTML 页面相对链接
    // compact=false 时包含：
    headings?: string[];
    navTrail?: string[];
    codeTemplates?: string[]; // 关联的代码模板 ID 列表
  }>;
}
```

---

## 2. GET /api/pages
文章查询与筛选接口。

**请求示例**：
```bash
# 精确获取单篇完整文章
curl -G --data-urlencode "id=binary-search" "$BASE_URL/api/pages"
# 按标签筛选列表
curl -G --data-urlencode "tag=图论,最短路" "$BASE_URL/api/pages"
```

**响应契约（单资源 ?id=xxx）**：
```ts
{
  id: string;
  title: string;
  path: string;
  url: string;
  description: string;
  tags: string[];
  categories: string[];
  frontMatter: Record<string, unknown>;
  headings: string[];
  excerpt: string;
  markdown: string;         // 原始 Markdown 正文
  html: string;             // 渲染后的 HTML
  text: string;             // 整篇文章的纯文本形式
}
```

**响应契约（列表查询）**：
```ts
{
  generatedAt: string;
  total: number;
  items: Array<{
    id: string;
    title: string;
    path: string;
    url: string;
    description: string;
    tags: string[];
    codeTemplates: string[];
  }>;
}
```

---

## 3. GET /api/codes
代码模板集中管理与查询接口。支持双向关联展示。

**请求示例**：
```bash
# 获取单个代码及完整源码
curl -G --data-urlencode "id=v-bcc" --data-urlencode "includeContent=true" "$BASE_URL/api/codes"
```

**响应契约（单条/列表中的元素）**：
```ts
{
  id: string;               // 唯一代码标识符
  path: string;             // 文件路径，如 "graph/v-bcc.cpp"
  url: string;              // 发布后的相对访问 URL，如 "/code/graph/v-bcc.cpp"
  description: string;      // 功能描述
  language: string;         // 代码语言，如 "cpp"
  tags: string[];           // 算法标签
  articles: Array<{         // 双向映射：引用该代码的所有文章
    id: string;
    title: string;
    path: string;
    url: string;
  }>;
  content?: string;         // 当 ?includeContent=true 时返回源码正文
}
```

---

## 4. GET /api/tags
标签统计分类统计与发现接口。

**响应契约**：
```ts
{
  generatedAt: string;
  articleTags: Array<{ tag: string; count: number }>;
  codeTags: Array<{ tag: string; count: number }>;
}
```
