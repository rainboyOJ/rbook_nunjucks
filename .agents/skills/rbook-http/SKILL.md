---
name: rbook-http
description: 使用 rbook 动态算法电子书的只读 HTTP API 检索、读取文章、获取代码模板和整理解题上下文。用户询问本算法电子书内容、需要定位文章、需要根据文章和模板代码写 OJ 题解或 C++ 代码、需要通过 HTTP 读取 rbook 内容时，应优先使用本技能。
---

# rbook HTTP 使用指南

本项目把算法电子书暴露为简洁的 RESTful 只读 HTTP API。它通过简短且全网唯一的 `id` 进行资源定位，同时支持双向关联，把代码模板与引用了该模板的文章进行双向映射。

## 基础配置

优先使用环境变量 `RBOOK_BASE_URL`：

```bash
export RBOOK_BASE_URL="http://127.0.0.1:3000"
```

线上部署地址：
```bash
export RBOOK_BASE_URL="https://rbook2.roj.ac.cn"
```

## 推荐统一命令行客户端（rbook.py）

本项目提供了一个全能客户端 CLI 脚本 `scripts/rbook.py`，替代了原有繁杂的旧版 python 脚本。
所有响应统一为 JSON 结构。

### 健康检查
```bash
python3 scripts/rbook.py health
```

### 获取精简目录
```bash
python3 scripts/rbook.py catalog --compact
```

### 按元数据查找文章
```bash
python3 scripts/rbook.py find "kmp 字符串"
python3 scripts/rbook.py find "动态规划 背包" --limit 10
```

`find` 只匹配文章的 ID、标题、描述和标签。多个关键词采用 AND 语义；确定文章 ID 后，再用 `pages --id` 读取全文。

### 按 ID 查询文章详情
```bash
python3 scripts/rbook.py pages --id binary-search
```

### 按标签筛选文章或代码
```bash
python3 scripts/rbook.py pages --tag 图论,双指针
python3 scripts/rbook.py codes --tag 差分
```

### 查看模板源码及其关联的文章
```bash
python3 scripts/rbook.py code v-bcc --content
```

### 统计所有文章标签与代码标签
```bash
python3 scripts/rbook.py tags
```

---

## HTTP RESTful API 概览

| 请求方法 | 路径 | 描述 | 常用查询参数 |
|----------|------|------|--------------|
| `GET` | `/api/health` | 检查服务状态 | - |
| `GET` | `/api/help` | 获取文档 | `format=html\|md` |
| `GET` | `/api/site` | 获取站点基础信息 | - |
| `GET` | `/api/catalog` | 获取全量或精简文章目录 | `compact=true` |
| `GET` | `/api/pages` | 查询单篇文章或标签筛选列表 | `id=xxx`, `tag=xxx`, `limit=50`, `offset=0` |
| `GET` | `/api/codes` | 查询模板或标签筛选列表 | `id=xxx`, `tag=xxx`, `includeContent=true` |
| `GET` | `/api/tags` | 获取文章标签及代码标签的词频统计 | - |

## 回答与解题最佳规范
1. **题解引用**：AI 编写题解时，必须利用 `api/codes` 获取对应的关联文章 `articles` 或通过 `api/pages` 查询，在生成的内容最后按格式进行标准引用，如 `[点双连通分量](https://rbook2.roj.ac.cn/graph/bcc/index.html)`。
2. **源码保持格式一致**：AI 在给用户出题解程序时，先通过 `api/codes?id=<id>&includeContent=true` 读取当前书站推荐的代码风格模板并作为基底展开，统一代码书写风格。
3. **安全路径限制**：API 和客户端均对所有资源执行了严格隔离，无法通过 `../` 越权，只允许访问 `book/pages` 和 `book/code` 下的标准内容。
