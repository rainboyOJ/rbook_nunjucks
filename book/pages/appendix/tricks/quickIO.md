---
id: "quick-io"
title: "quickIO"
date: 2025-11-04 11:32
toc: true
tags: ["tricks","template"]
code_template:
  - title: 快速输入输出
    desc: "使用 getchar/putchar 实现整数快读快写"
    tags: ["输入输出", "快读", "工具"]
    code: /code/utils/quick_io.cpp
categories: ["附录", "工具"]
---

[[TOC]]


## 一句话工具

quickIO 用 `getchar` 和 `putchar` 手写整数输入输出，在数据量很大时降低标准流格式化带来的常数。

## 什么时候需要

大多数题目直接使用下面这行已经足够快：

```cpp
ios::sync_with_stdio(false);
cin.tie(nullptr);
```

但如果题目输入输出规模非常大，例如有几十万到几百万个整数，手写快读快写仍然有价值。它适合：

- 大量整数读入。
- 大量整数输出。
- 对常数比较敏感的模板题。
- 本地对拍时快速生成或读取大数据。

它不适合读入字符串、整行文本、浮点数；这些场景应继续使用标准库接口，或者单独写对应版本。

## 核心接口

模板提供四类接口：

```cpp
read(x);
read(a, b, c);
write(x);
write(a, b, c);
```

`read` 支持整数类型，包括 `int`、`long long`。`write` 只输出数字本身，不会自动补空格或换行。

例如：

```cpp
int n;
long long x;
read(n, x);
write(n);
putchar('\n');
write(x);
```

## 工作过程

`read(x)` 的核心逻辑分两步：

1. 跳过非数字字符，同时记录负号。
2. 连续读取数字字符，把十进制位累加成整数。

`write(x)` 的核心逻辑也分两步：

1. 如果是负数，先输出 `-`。
2. 反复取个位数字放进缓冲区，再倒序输出。

这个过程避免了 `iostream` 的格式化开销，所以在纯整数读写时常数更小。

## 代码实现

模板文件位置：`/code/utils/quick_io.cpp`。

@include-code(/code/utils/quick_io.cpp, cpp)

## 常见改法

### 自动输出空格或换行

当前 `write(a, b, c)` 会连续输出三个数字，中间没有分隔符。如果你想输出空格，需要自己写：

```cpp
write(a);
putchar(' ');
write(b);
putchar('\n');
```

也可以封装一个带换行的函数：

```cpp
template <typename T>
inline void writeln(T x) {
    write(x);
    putchar('\n');
}
```

### 使用 getchar_unlocked

在部分 Linux 环境中，`getchar_unlocked` 和 `putchar_unlocked` 会更快。但它们不是 C++ 标准接口，在某些平台不可用。

如果只在 Linux OJ 上提交，可以把模板里的 `getchar`/`putchar` 替换为对应的 unlocked 版本；如果要保证跨平台，保持当前写法更稳。

### 处理 EOF

当前模板默认输入合法，适合竞赛题。如果需要在读到文件末尾时停止，可以把 `read` 改成返回 `bool`，遇到 `EOF` 时返回 `false`。

## 注意事项

- 不要在同一个程序里混用大量 `cin/cout` 和 `getchar/putchar`，尤其是在关闭同步之后，缓冲行为容易让人困惑。
- `write` 不会自动输出空格和换行。
- 当前模板只处理整数，不处理浮点数和字符串。
- 如果读入的是无符号类型，却遇到负数输入，结果没有实际意义。
- 极端情况下，最小负数取相反数可能溢出；普通竞赛整数范围通常不会踩到这个边界，但写通用库时要单独处理。

## 推荐用法

把 quickIO 当作“可选加速件”。平时优先使用标准流，只有当输入输出成为瓶颈，或者模板题明确卡常数时，再切换到这份代码。
