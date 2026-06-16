---
id: "heap"
title: "堆"
date: 2026-01-01 12:20
toc: true
tags: [""]
categories: [""]
code_template:
  - title: 堆
    desc: "堆"
    tags: ["集合最值"]
    code: /code/data-struture/heap堆.cpp
---

[[TOC]]


这是一个使用 `struct` 实现的通用小根堆模板。

### C++ Struct 版小根堆模板

@include-code(/code/data-struture/heap堆.cpp,cpp)

### 为什么这个版本心智负担最低？

1. **物理直觉强**：
   - **Up**: 气球（小值）往上飘。
   - **Down**: 石头（大值）往下沉，而且往更低（更小值）的方向滚。
2. **公式简单**：
   - 不用处理 `+1` / `-1` 的下标偏移，因为 `h[0]` 被废弃了。
   - 只有简单的 `u/2` (找爸爸), `u*2` (找左娃), `u*2+1` (找右娃)。
3. **Struct 优势**：
   - 如果你在比赛中调试，可以直接打印 `heap.h[i]` 来查看内部数组状态，没有任何 `private` 权限阻挡。

## 题目

- [[problem: luogu,P3378]]