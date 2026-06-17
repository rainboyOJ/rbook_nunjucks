---
id: "contest-code-template"
title: "竞赛代码模板"
date: 2026-06-17 00:00
toc: true
tags: ["模板", "工具"]
categories: ["附录"]
code_template:
  - title: 竞赛代码模板
    desc: "通用 C++ 竞赛代码骨架，包含调试宏和 quickIO 引用"
    tags: ["模板", "C++", "工具"]
    code: /code/template/template.cpp
---

[[TOC]]

## 一句话工具

竞赛代码模板是一份“开题骨架”：先放好头文件、类型别名、调试宏、快读快写和主函数结构，写题时只补核心逻辑。

## 模板解决的问题

比赛时每道题都要重复写一些固定内容：

- 头文件和命名空间。
- 常用类型别名。
- 全局数组和常量。
- 输入函数。
- 调试输出。
- `main` 函数骨架。

这些内容本身不难，但重复手写会浪费注意力，也容易出现细小错误。通用模板的目标是把这些固定部分提前准备好，让写题时把精力放在建模和核心算法上。

## 模板结构

当前模板大致分成五块：

1. 文件头信息：记录作者、题目、OJ、描述等。
2. 基础环境：`bits/stdc++.h`、`using namespace std`、`typedef long long ll`。
3. 调试宏：本地开启，提交时可用 `ONLINE_JUDGE` 或 `NO_DEBUG` 屏蔽。
4. 全局数据区：常量、数组、`n`、`m` 等常见变量。
5. 主函数骨架：读入、处理、输出。

其中 quickIO 通过模板引用接入：

```cpp
{{include "code/utils/quick_io.cpp"}}
```

如果你的代码模板系统会展开这个引用，就能自动把快读快写插入当前位置；如果只是手动复制，则需要把 quickIO 对应代码一并复制进去。

## 使用步骤

1. 复制通用模板。
2. 根据题目替换文件头里的 `oj`、`title`、`description` 等占位信息。
3. 修改 `maxn`、全局数组和读入变量。
4. 在 `init` 或对应位置读入数据。
5. 在 `main` 中补上核心算法流程。
6. 提交前确认调试输出已关闭。

## 代码实现

模板文件位置：`/code/template/template.cpp`。

@include-code(/code/template/template.cpp, cpp)

## 常见改法

### 不使用 quickIO

如果题目输入规模不大，可以删除 quickIO 引用，改用标准流：

```cpp
ios::sync_with_stdio(false);
cin.tie(nullptr);
```

这样代码更短，也更适合字符串、浮点数、混合输入等场景。

### 调整调试宏

模板里通过 `NO_DEBUG` 和 `ONLINE_JUDGE` 控制调试输出。常见做法是本地默认开启，提交时自动关闭：

```cpp
#if defined(ONLINE_JUDGE)
#define log(...)
#endif
```

如果某个 OJ 没有定义 `ONLINE_JUDGE`，可以在提交前手动加上：

```cpp
#define NO_DEBUG
```

### 改成多测试用例结构

如果题目有多组数据，可以把核心逻辑封装成 `solve()`：

```cpp
void solve() {
    // read one test case
    // solve one test case
}

int main() {
    int T;
    read(T);
    while (T--) solve();
    return 0;
}
```

## 注意事项

- 通用模板不是越大越好，只保留高频、低风险、你真的熟悉的内容。
- `maxn` 要按题目约束调整，避免数组过小越界，也避免盲目开过大的全局数组。
- 如果模板里含有占位符，提交前必须替换干净。
- 调试宏不要把信息输出到正式答案里。
- 不同题目的输入输出特点不同，quickIO 和标准流不要机械套用。

## 维护方式

后续如果要修改通用头文件、常用类型别名、输入输出设置，请直接修改上面的代码文件。文章只负责解释模板的结构和使用方法，真正可复制的代码以 `book/code/template/template.cpp` 为准。
