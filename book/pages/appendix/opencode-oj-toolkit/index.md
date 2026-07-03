---
id: "opencode-oj-toolkit"
title: "OpenCode OJ 学习工具箱"
date: 2026-07-02 21:00
toc: true
tags: ["工具", "OJ", "opencode", "工作流"]
categories: ["附录"]
---

[[TOC]]

## 背景

在算法竞赛学习中，刷题是最重要的环节。一个典型的刷题流程包括：读题 → 理解题意 → 设计暴力解 → 分析瓶颈 → 推导优化 → 实现正解 → 跑样例 → 对拍验证 → 复盘总结。

[OpenCode](https://opencode.ai) 是一个 AI 编程助手。本项目附带了一套自包含的 **OpenCode OJ 学习工具箱**（位于项目根目录的 `opencode/`），将上述刷题流程固化为 opencode 的命令、skill 和 CLI 工具，让 AI agent 按照学习优先（而非代写优先）的方式辅助你解题。

工具包包含三个层次：

- **`/oj-*` 命令**：opencode 中的 12 个对话命令
- **`oj` CLI**：独立的命令行工具，不依赖 opencode
- **Skill**：约束 AI 行为的规则文件

## 安装

将 `opencode/` 部署到目标项目的 `.opencode/`：

```bash
cp -r opencode/ <目标项目根>/.opencode/
chmod +x .opencode/oj-tools/bin/oj
# 若目标已有 .opencode/opencode.json，合并 plugin 数组
```

详细步骤（包括白名单复制、`opencode.json` 合并、验证）见 [`opencode/install-ai.md`](https://github.com/rainboyOJ/rbook_nunjucks/blob/main/opencode/install-ai.md)，那是给 AI agent 读取的完整安装手册。

部署后重启 opencode，它会在启动时自动从 GitHub 拉取 `superpowers` 插件（无需手动 `npm install`）。

## 学习工作流

工具包默认的学习顺序：

```
题意理解 → 暴力解 → 瓶颈分析 → 优化观察 → 正解实现 → 样例检查 → 对拍验证 → 复盘总结
```

AI 不会跳过中间步骤直接给出完整优化代码。除非你明确进入参考阶段（使用 `/oj-reference`）。

## `/oj-*` 命令速查

| 命令 | 作用 | 典型用法 |
|------|------|---------|
| `/oj-new P1001` | 创建题目目录结构 | `/oj-new P1001` |
| `/oj-fetch P1001` | 下载洛谷题面和样例 | `/oj-fetch P1001` |
| `/oj-status` | 检查当前题目材料状态 | `/oj-status` |
| `/oj-hint` | 给出分层提示（不给完整解） | `/oj-hint` |
| `/oj-brute` | 写/改 `brute.cpp` | `/oj-brute` |
| `/oj-gen` | 写/改 `gen.cpp` | `/oj-gen` |
| `/oj-run` | 编译运行 `main.cpp` | `/oj-run` |
| `/oj-sample` | 编译并检查样例 | `/oj-sample` |
| `/oj-duipai 100` | 对拍 100 组数据 | `/oj-duipai 100` |
| `/oj-review` | 审查学生代码 | `/oj-review` |
| `/oj-reference` | 最后阶段给参考解 | `/oj-reference` |
| `/oj-help` | 显示帮助信息 | `/oj-help` |

### 各命令详解

**`/oj-new <题号>`**：在 `problems/<oj>/<id>/` 下创建题目目录，生成 `index.md`、`main.cpp`、`brute.cpp`、`gen.cpp` 四个模板文件和一个 `problem-analysis-workspace/` 目录（用于存放分析笔记）。不会覆盖已有文件。

**`/oj-fetch <题号>`**：从洛谷抓取题目描述和输入输出样例，写入 `problem.md` 和 `in`/`out` 文件。默认不覆盖已有内容。首次做题前必须执行。

**`/oj-status`**：检查当前 `problems/` 目录的进展——哪些文件存在，哪些缺少，是否已有对拍失败记录。

**`/oj-hint`**：按照"暴力 → 瓶颈 → 观察 → 优化"链式给出提示，从不一次性丢出完整解法。执行前会先读 `problem.md` 和已有代码。

**`/oj-brute`**：创建或修改 `brute.cpp`（小数据暴力解）。暴力解必须和 `main.cpp` 输入格式一致、结果可信任，用来验证正解的正确性。

**`/oj-gen`**：创建或修改 `gen.cpp`（随机数据生成器）。用 C++ 生成随机测试数据，生成的数据范围应适合 `brute.cpp` 运行。

**`/oj-run`**：编译 `main.cpp` 并运行。可指定 `--source` 运行其他文件，`--input` 指定输入，`--timeout` 限制运行时间。

**`/oj-sample`**：编译 `main.cpp`，用 `in`/`out` 目录中的样例数据进行测试，逐组对比输出，报告差异。

**`/oj-duipai <组数>`**：三文件对拍：`main.cpp`（正解） vs `brute.cpp`（暴力） vs `gen.cpp`（生成器）。运行指定组数，遇差异立即停止，保存数据到 `duipai-failed/`。

**`/oj-review`**：审查学生写的代码。检查顺序：正确性 → 边界 → 复杂度 → 实现风险 → 风格。不会整体重写，只给针对性修复建议。

**`/oj-reference`**：仅在学习流程走完后给出完整参考解。提供思路、正确性理由、实现细节、C++17 代码，以及建议的验证命令。

**`/oj-help`**：显示所有命令的简要说明和学习顺序。

## `oj` CLI 工具

`oj` 是独立于 opencode 的命令行工具，位于 `.opencode/oj-tools/bin/oj`。它使用 python3 标准库，无需安装任何 pip 包。

```bash
.opencode/oj-tools/bin/oj --help
```

支持的子命令：

| 子命令 | 作用 |
|--------|------|
| `oj new <oj> <id>` | 创建题目目录 |
| `oj fetch <id>` | 从洛谷下载数据 |
| `oj status` | 查看题目状态 |
| `oj run` | 编译运行 `main.cpp` |
| `oj sample` | 测试样例 |
| `oj duipai <n>` | 对拍 n 组 |
| `oj gen` | 创建 `gen.cpp` |

`oj` 自动向上寻找包含 `problems/` 或 `.opencode/` 的目录作为项目根，因此你可以在题目的子目录中直接运行。

`oj sample` 和 `oj duipai` 使用 `g++ -std=c++17 -O2 -pipe` 编译，运行带 2 秒超时保护。

## Skill 机制

工具包部署后默认包含以下 skill，由 opencode 自动加载：

**`oj-student-coach`**：约束 AI 走学习优先的工作流——先理解题意，再暴力解，再优化，不给完整正解。

**`oj-cpp-rbook-style`**：约束 C++ 代码风格——C++17、全局数组、清晰的 `for` 循环、有用的中文注释、避免 lambda/结构化绑定/复杂宏。

**`rbook-http`**：通过 HTTP API 查询本 rbook 电子书内容（文章、代码模板、搜索），让 AI agent 能根据已有文章和模板代码回答问题或写 OJ 题解。安装时从 `.agents/skills/rbook-http/` 单独复制到 `.opencode/skills/`。

这三个 skill 在 `.opencode/skills/` 下，可以被 `/oj-*` 命令和 AI 对话共同使用。

## 项目结构约定

题目目录遵循统一格式，`/oj-new` 会按此自动创建：

```text
problems/<oj>/<problem_id>/
├── problem.md           # 题面（由 /oj-fetch 生成）
├── index.md             # 题目元信息和笔记
├── main.cpp             # 正解
├── brute.cpp            # 暴力解
├── gen.cpp              # 数据生成器
├── in/                  # 输入样例
├── out/                 # 输出样例
├── in1/ out1/ …         # 自定义测试数据
├── duipai-failed/       # 对拍失败数据（自动生成）
└── problem-analysis-workspace/  # 分析笔记
```

## 最佳实践

1. **先 fetch 再做题**：拿到新题先用 `/oj-fetch` 下载题面和样例。
2. **保持三文件同步**：`main.cpp`、`brute.cpp`、`gen.cpp` 的输入输出格式必须一致。
3. **对拍是最后防线**：写完正解后跑 `/oj-duipai 100`，发现差异就去 `duipai-failed/` 查看反例。
4. **善用 `/oj-hint`**：卡住时让 AI 引导思路，而不是直接看题解。
5. **复盘总结**：AC 后用 `/oj-reference` 对照参考实现，阅读 `problem-analysis-workspace/` 里的分析笔记。
6. **技能自动生效**：只要 `.opencode/` 部署正确，opencode 会自动加载 skill，无需手动配置。
